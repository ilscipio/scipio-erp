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
/*
 * SCIPIO: Manufacturing capacity planning services: work center load and a manufacturing dashboard summary.
 */
package com.ilscipio.scipio.manufacturing.planning;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.manufacturing.techdata.TechDataServices;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Manufacturing capacity planning services.
 */
public class PlanningServices {

    private static final String MODULE = PlanningServices.class.getName();

    private static final List<String> OPEN_TASK_STATUSES = UtilMisc.toList("PRUN_CREATED", "PRUN_SCHEDULED", "PRUN_DOC_PRINTED", "PRUN_RUNNING");
    private static final List<String> CLOSED_TASK_STATUSES = UtilMisc.toList("PRUN_COMPLETED", "PRUN_CLOSED");
    private static final List<String> ALL_RUN_STATUSES = UtilMisc.toList("PRUN_CREATED", "PRUN_SCHEDULED", "PRUN_DOC_PRINTED", "PRUN_RUNNING", "PRUN_COMPLETED", "PRUN_CLOSED", "PRUN_CANCELLED");

    /** Returns the capacity and load of production equipment work centers, per day and in total, for a date range. */
    public static Map<String, Object> getWorkCenterLoad(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        try {
            String fixedAssetId = (String) context.get("fixedAssetId");
            String facilityId = (String) context.get("facilityId");
            Timestamp fromDate = (Timestamp) context.get("fromDate");
            Timestamp thruDate = (Timestamp) context.get("thruDate");
            Boolean includeClosed = (Boolean) context.get("includeClosed");
            if (fromDate == null) {
                fromDate = UtilDateTime.getDayStart(UtilDateTime.nowTimestamp());
            }
            if (thruDate == null) {
                thruDate = UtilDateTime.addDaysToTimestamp(fromDate, 14);
            }
            if (includeClosed == null) {
                includeClosed = Boolean.FALSE;
            }

            List<String> statusIds = new ArrayList<>(OPEN_TASK_STATUSES);
            if (Boolean.TRUE.equals(includeClosed)) {
                statusIds.addAll(CLOSED_TASK_STATUSES);
            }

            List<Timestamp> days = new ArrayList<>();
            for (Timestamp day = fromDate; day.before(thruDate); day = UtilDateTime.addDaysToTimestamp(day, 1)) {
                days.add(day);
            }

            List<EntityCondition> wcConds = new ArrayList<>();
            wcConds.add(EntityCondition.makeCondition("fixedAssetTypeId", EntityOperator.IN, UtilMisc.toList("PRODUCTION_EQUIPMENT", "GROUP_EQUIPMENT")));
            if (UtilValidate.isNotEmpty(fixedAssetId)) {
                wcConds.add(EntityCondition.makeCondition("fixedAssetId", fixedAssetId));
            }
            if (UtilValidate.isNotEmpty(facilityId)) {
                wcConds.add(EntityCondition.makeCondition("locatedAtFacilityId", facilityId));
            }
            List<GenericValue> workCenterEntities = EntityQuery.use(delegator).from("FixedAsset").where(wcConds).cache(true).queryList();
            if (UtilValidate.isEmpty(fixedAssetId)) {
                // SCIPIO: a line's members are rolled up into the line's own row below, not listed on their own.
                List<GenericValue> topLevelEntities = new ArrayList<>();
                for (GenericValue workCenter : workCenterEntities) {
                    if (UtilValidate.isEmpty(workCenter.getString("parentFixedAssetId"))) {
                        topLevelEntities.add(workCenter);
                    }
                }
                workCenterEntities = topLevelEntities;
            }

            List<Map<String, Object>> workCenters = new ArrayList<>();
            List<Map<String, Object>> loadRows = new ArrayList<>();
            List<Map<String, Object>> tasks = new ArrayList<>();

            for (GenericValue workCenter : workCenterEntities) {
                String wcFixedAssetId = workCenter.getString("fixedAssetId");
                // SCIPIO: a production line is a FixedAsset whose members point to it via parentFixedAssetId.
                List<GenericValue> members = EntityQuery.use(delegator).from("FixedAsset")
                        .where("parentFixedAssetId", wcFixedAssetId).orderBy("fixedAssetId").queryList();
                boolean isLine = UtilValidate.isNotEmpty(members);

                GenericValue techDataCalendar = resolveCalendar(delegator, workCenter);
                String calendarId = techDataCalendar != null ? techDataCalendar.getString("calendarId") : "DEFAULT";

                Map<Timestamp, Double> capacityByDay = new LinkedHashMap<>();
                for (Timestamp day : days) {
                    capacityByDay.put(day, 0.0);
                }
                double totalCapacityMinutes = 0.0;
                if (isLine) {
                    // SCIPIO: capacity of a line = sum of its members' capacity. Each member uses its own
                    // calendar, falling back to the line's calendar when the member has none (see resolveCalendar()).
                    for (GenericValue member : members) {
                        GenericValue memberCalendar = resolveCalendar(delegator, member);
                        GenericValue memberCalendarWeek = memberCalendar != null ? memberCalendar.getRelatedOne("TechDataCalendarWeek", true) : null;
                        for (Timestamp day : days) {
                            double capacityMinutes = 0.0;
                            if (memberCalendar != null && memberCalendarWeek != null) {
                                Map<String, Object> dayInfo = TechDataServices.getDayCapacity(memberCalendar, memberCalendarWeek, day);
                                Double capacity = (Double) dayInfo.get("capacity");
                                capacityMinutes = (capacity != null ? capacity : 0.0) / 60000.0;
                            }
                            capacityByDay.put(day, capacityByDay.get(day) + capacityMinutes);
                            totalCapacityMinutes += capacityMinutes;
                        }
                    }
                } else {
                    GenericValue techDataCalendarWeek = techDataCalendar != null ? techDataCalendar.getRelatedOne("TechDataCalendarWeek", true) : null;
                    for (Timestamp day : days) {
                        double capacityMinutes = 0.0;
                        if (techDataCalendar != null && techDataCalendarWeek != null) {
                            Map<String, Object> dayInfo = TechDataServices.getDayCapacity(techDataCalendar, techDataCalendarWeek, day);
                            Double capacity = (Double) dayInfo.get("capacity");
                            capacityMinutes = (capacity != null ? capacity : 0.0) / 60000.0;
                        }
                        capacityByDay.put(day, capacityMinutes);
                        totalCapacityMinutes += capacityMinutes;
                    }
                }

                Map<Timestamp, Double> loadByDay = new LinkedHashMap<>();
                for (Timestamp day : days) {
                    loadByDay.put(day, 0.0);
                }

                // SCIPIO: load of a line = the line's own tasks plus the tasks of every member machine.
                List<String> loadFixedAssetIds = new ArrayList<>();
                loadFixedAssetIds.add(wcFixedAssetId);
                for (GenericValue member : members) {
                    loadFixedAssetIds.add(member.getString("fixedAssetId"));
                }
                List<EntityCondition> taskConds = UtilMisc.toList(
                        EntityCondition.makeCondition("workEffortTypeId", "PROD_ORDER_TASK"),
                        isLine ? EntityCondition.makeCondition("fixedAssetId", EntityOperator.IN, loadFixedAssetIds)
                                : EntityCondition.makeCondition("fixedAssetId", wcFixedAssetId),
                        EntityCondition.makeCondition("currentStatusId", EntityOperator.IN, statusIds),
                        EntityCondition.makeCondition("estimatedStartDate", EntityOperator.LESS_THAN, thruDate),
                        EntityCondition.makeCondition("estimatedCompletionDate", EntityOperator.GREATER_THAN, fromDate));
                List<GenericValue> taskEntities = EntityQuery.use(delegator).from("WorkEffort").where(taskConds).queryList();

                double totalLoadMinutes = 0.0;
                for (GenericValue task : taskEntities) {
                    Timestamp taskStart = task.getTimestamp("estimatedStartDate");
                    Timestamp taskEnd = task.getTimestamp("estimatedCompletionDate");
                    String parentId = task.getString("workEffortParentId");
                    GenericValue parentRun = UtilValidate.isNotEmpty(parentId)
                            ? EntityQuery.use(delegator).from("WorkEffort").where("workEffortId", parentId).queryOne() : null;

                    double taskMinutes = computeTaskMinutes(task, parentRun);

                    Map<Timestamp, Double> spread = spreadLoad(taskStart, taskEnd, taskMinutes, days);
                    for (Map.Entry<Timestamp, Double> entry : spread.entrySet()) {
                        double clipped = loadByDay.get(entry.getKey()) + entry.getValue();
                        loadByDay.put(entry.getKey(), clipped);
                        totalLoadMinutes += entry.getValue();
                    }

                    Map<String, Object> taskMap = new LinkedHashMap<>();
                    taskMap.put("workEffortId", task.getString("workEffortId"));
                    taskMap.put("workEffortName", task.getString("workEffortName"));
                    taskMap.put("productionRunId", parentRun != null ? parentRun.getString("workEffortId") : null);
                    taskMap.put("productionRunName", parentRun != null ? parentRun.getString("workEffortName") : null);
                    taskMap.put("productId", parentRun != null ? getRunProductId(delegator, parentRun.getString("workEffortId")) : null);
                    taskMap.put("fixedAssetId", task.getString("fixedAssetId"));
                    taskMap.put("workCenterFixedAssetId", wcFixedAssetId);
                    taskMap.put("currentStatusId", task.getString("currentStatusId"));
                    taskMap.put("estimatedStartDate", taskStart);
                    taskMap.put("estimatedCompletionDate", taskEnd);
                    taskMap.put("loadMinutes", taskMinutes);
                    taskMap.put("priority", task.getLong("priority"));
                    tasks.add(taskMap);
                }

                for (Timestamp day : days) {
                    double capacityMinutes = capacityByDay.get(day);
                    double loadMinutes = loadByDay.get(day);
                    BigDecimal loadPercent = capacityMinutes > 0
                            ? BigDecimal.valueOf(loadMinutes / capacityMinutes * 100.0).setScale(1, RoundingMode.HALF_UP)
                            : BigDecimal.ZERO.setScale(1, RoundingMode.HALF_UP);
                    Map<String, Object> loadRow = new LinkedHashMap<>();
                    loadRow.put("fixedAssetId", wcFixedAssetId);
                    loadRow.put("day", day);
                    loadRow.put("capacityMinutes", capacityMinutes);
                    loadRow.put("loadMinutes", loadMinutes);
                    loadRow.put("loadPercent", loadPercent);
                    loadRows.add(loadRow);
                }

                Map<String, Object> workCenterMap = new LinkedHashMap<>();
                workCenterMap.put("fixedAssetId", wcFixedAssetId);
                workCenterMap.put("fixedAssetName", workCenter.getString("fixedAssetName"));
                workCenterMap.put("calendarId", calendarId);
                workCenterMap.put("totalCapacityMinutes", totalCapacityMinutes);
                workCenterMap.put("totalLoadMinutes", totalLoadMinutes);
                workCenterMap.put("loadPercent", totalCapacityMinutes > 0 ? (totalLoadMinutes / totalCapacityMinutes * 100.0) : 0.0);
                workCenterMap.put("isLine", isLine);
                List<Map<String, Object>> memberMaps = new ArrayList<>();
                for (GenericValue member : members) {
                    Map<String, Object> memberMap = new LinkedHashMap<>();
                    memberMap.put("fixedAssetId", member.get("fixedAssetId"));
                    memberMap.put("fixedAssetName", member.get("fixedAssetName"));
                    memberMaps.add(memberMap);
                }
                workCenterMap.put("members", memberMaps);
                workCenters.add(workCenterMap);
            }

            tasks.sort(Comparator
                    .comparing((Map<String, Object> m) -> (String) m.get("fixedAssetId"), Comparator.nullsLast(Comparator.naturalOrder()))
                    .thenComparing(m -> (Timestamp) m.get("estimatedStartDate"), Comparator.nullsLast(Comparator.naturalOrder())));

            Map<String, Object> result = ServiceUtil.returnSuccess();
            result.put("workCenters", workCenters);
            result.put("days", days);
            result.put("loadRows", loadRows);
            result.put("tasks", tasks);
            return result;
        } catch (GenericEntityException e) {
            Debug.logError(e, MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
    }

    /** Returns a manufacturing summary: run status counts, late/upcoming runs, running tasks, shortages, MRP proposals and work center load. */
    public static Map<String, Object> getManufacturingDashboard(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        try {
            String facilityId = (String) context.get("facilityId");
            Integer days = (Integer) context.get("days");
            if (days == null) {
                days = 7;
            }
            GenericValue userLogin = (GenericValue) context.get("userLogin");
            Timestamp now = UtilDateTime.nowTimestamp();

            // runCounts
            Map<String, Object> runCounts = new LinkedHashMap<>();
            long total = 0;
            for (String statusId : ALL_RUN_STATUSES) {
                List<EntityCondition> conds = new ArrayList<>();
                conds.add(EntityCondition.makeCondition("workEffortTypeId", "PROD_ORDER_HEADER"));
                conds.add(EntityCondition.makeCondition("currentStatusId", statusId));
                if (UtilValidate.isNotEmpty(facilityId)) {
                    conds.add(EntityCondition.makeCondition("facilityId", facilityId));
                }
                long count = EntityQuery.use(delegator).from("WorkEffort").where(conds).queryCount();
                runCounts.put(statusId, count);
                total += count;
            }
            runCounts.put("total", total);

            // lateRuns
            List<EntityCondition> lateConds = UtilMisc.toList(
                    EntityCondition.makeCondition("workEffortTypeId", "PROD_ORDER_HEADER"),
                    EntityCondition.makeCondition("currentStatusId", EntityOperator.IN, OPEN_TASK_STATUSES),
                    EntityCondition.makeCondition("estimatedCompletionDate", EntityOperator.LESS_THAN, now));
            List<GenericValue> lateRunEntities = EntityQuery.use(delegator).from("WorkEffort").where(lateConds)
                    .orderBy("estimatedCompletionDate").queryList();
            List<Map<String, Object>> lateRuns = new ArrayList<>();
            for (GenericValue run : lateRunEntities) {
                if (lateRuns.size() >= 20) {
                    break;
                }
                lateRuns.add(buildRunSummary(delegator, run, now));
            }

            // runningTasks
            List<EntityCondition> runningConds = UtilMisc.toList(
                    EntityCondition.makeCondition("workEffortTypeId", "PROD_ORDER_TASK"),
                    EntityCondition.makeCondition("currentStatusId", "PRUN_RUNNING"));
            List<GenericValue> runningTaskEntities = EntityQuery.use(delegator).from("WorkEffort").where(runningConds).queryList();
            List<Map<String, Object>> runningTasks = new ArrayList<>();
            for (GenericValue task : runningTaskEntities) {
                Map<String, Object> taskMap = new LinkedHashMap<>();
                taskMap.put("workEffortId", task.getString("workEffortId"));
                taskMap.put("workEffortName", task.getString("workEffortName"));
                taskMap.put("workEffortParentId", task.getString("workEffortParentId"));
                taskMap.put("fixedAssetId", task.getString("fixedAssetId"));
                taskMap.put("actualStartDate", task.getTimestamp("actualStartDate"));
                taskMap.put("quantityProduced", task.getBigDecimal("quantityProduced"));
                taskMap.put("quantityRejected", task.getBigDecimal("quantityRejected"));
                runningTasks.add(taskMap);
            }

            // upcomingRuns
            Timestamp upcomingThru = UtilDateTime.addDaysToTimestamp(now, days);
            List<EntityCondition> upcomingConds = UtilMisc.toList(
                    EntityCondition.makeCondition("workEffortTypeId", "PROD_ORDER_HEADER"),
                    EntityCondition.makeCondition("currentStatusId", EntityOperator.NOT_IN, UtilMisc.toList("PRUN_CLOSED", "PRUN_CANCELLED")),
                    EntityCondition.makeCondition("estimatedStartDate", EntityOperator.GREATER_THAN_EQUAL_TO, now),
                    EntityCondition.makeCondition("estimatedStartDate", EntityOperator.LESS_THAN_EQUAL_TO, upcomingThru));
            List<GenericValue> upcomingRunEntities = EntityQuery.use(delegator).from("WorkEffort").where(upcomingConds)
                    .orderBy("estimatedStartDate").queryList();
            List<Map<String, Object>> upcomingRuns = new ArrayList<>();
            for (GenericValue run : upcomingRunEntities) {
                if (upcomingRuns.size() >= 20) {
                    break;
                }
                Map<String, Object> runMap = buildRunSummary(delegator, run, now);
                runMap.put("estimatedStartDate", run.getTimestamp("estimatedStartDate"));
                upcomingRuns.add(runMap);
            }

            // shortages
            List<EntityCondition> openRunConds = new ArrayList<>();
            openRunConds.add(EntityCondition.makeCondition("workEffortTypeId", "PROD_ORDER_HEADER"));
            openRunConds.add(EntityCondition.makeCondition("currentStatusId", EntityOperator.IN,
                    UtilMisc.toList("PRUN_CREATED", "PRUN_SCHEDULED", "PRUN_DOC_PRINTED")));
            if (UtilValidate.isNotEmpty(facilityId)) {
                openRunConds.add(EntityCondition.makeCondition("facilityId", facilityId));
            }
            List<GenericValue> openRuns = EntityQuery.use(delegator).from("WorkEffort").where(openRunConds).queryList();

            // key: facilityId + "|" + productId -> needed quantity
            Map<String, BigDecimal> neededByKey = new LinkedHashMap<>();
            Map<String, String> facilityByKey = new LinkedHashMap<>();
            Map<String, String> productByKey = new LinkedHashMap<>();
            for (GenericValue run : openRuns) {
                String runFacilityId = run.getString("facilityId");
                List<GenericValue> runTasks = EntityQuery.use(delegator).from("WorkEffort")
                        .where("workEffortParentId", run.getString("workEffortId")).queryList();
                for (GenericValue task : runTasks) {
                    List<GenericValue> materials = EntityQuery.use(delegator).from("WorkEffortGoodStandard")
                            .where("workEffortId", task.getString("workEffortId"), "workEffortGoodStdTypeId", "PRUNT_PROD_NEEDED")
                            .queryList();
                    for (GenericValue material : materials) {
                        String productId = material.getString("productId");
                        BigDecimal qty = material.getBigDecimal("estimatedQuantity");
                        if (qty == null) {
                            continue;
                        }
                        String key = runFacilityId + "|" + productId;
                        BigDecimal existing = neededByKey.get(key);
                        neededByKey.put(key, existing != null ? existing.add(qty) : qty);
                        facilityByKey.put(key, runFacilityId);
                        productByKey.put(key, productId);
                    }
                }
            }

            List<Map<String, Object>> shortages = new ArrayList<>();
            for (Map.Entry<String, BigDecimal> entry : neededByKey.entrySet()) {
                String key = entry.getKey();
                String productId = productByKey.get(key);
                String shortageFacilityId = facilityByKey.get(key);
                BigDecimal quantityNeeded = entry.getValue();
                if (UtilValidate.isEmpty(shortageFacilityId) || UtilValidate.isEmpty(productId)) {
                    continue;
                }
                Map<String, Object> availCtx = UtilMisc.toMap("productId", productId, "facilityId", shortageFacilityId, "userLogin", userLogin);
                Map<String, Object> availResult = dispatcher.runSync("getInventoryAvailableByFacility", availCtx);
                if (ServiceUtil.isError(availResult)) {
                    Debug.logWarning("Could not get inventory availability for product " + productId + ": " + ServiceUtil.getErrorMessage(availResult), MODULE);
                    continue;
                }
                BigDecimal quantityAvailable = (BigDecimal) availResult.get("availableToPromiseTotal");
                if (quantityAvailable == null) {
                    quantityAvailable = BigDecimal.ZERO;
                }
                if (quantityNeeded.compareTo(quantityAvailable) > 0) {
                    GenericValue product = EntityQuery.use(delegator).from("Product").where("productId", productId).cache(true).queryOne();
                    Map<String, Object> shortageMap = new LinkedHashMap<>();
                    shortageMap.put("productId", productId);
                    shortageMap.put("internalName", product != null ? product.getString("internalName") : null);
                    shortageMap.put("facilityId", shortageFacilityId);
                    shortageMap.put("quantityNeeded", quantityNeeded);
                    shortageMap.put("quantityAvailable", quantityAvailable);
                    shortageMap.put("shortage", quantityNeeded.subtract(quantityAvailable));
                    shortages.add(shortageMap);
                }
            }
            shortages.sort(Comparator.comparing((Map<String, Object> m) -> (BigDecimal) m.get("shortage")).reversed());
            if (shortages.size() > 50) {
                shortages = new ArrayList<>(shortages.subList(0, 50));
            }

            // mrpProposals
            List<EntityCondition> prodReqConds = new ArrayList<>();
            prodReqConds.add(EntityCondition.makeCondition("statusId", "REQ_PROPOSED"));
            prodReqConds.add(EntityCondition.makeCondition("requirementTypeId", "INTERNAL_REQUIREMENT"));
            if (UtilValidate.isNotEmpty(facilityId)) {
                prodReqConds.add(EntityCondition.makeCondition("facilityId", facilityId));
            }
            long productionRuns = EntityQuery.use(delegator).from("Requirement").where(prodReqConds).queryCount();

            List<EntityCondition> purchReqConds = new ArrayList<>();
            purchReqConds.add(EntityCondition.makeCondition("statusId", "REQ_PROPOSED"));
            purchReqConds.add(EntityCondition.makeCondition("requirementTypeId", "PRODUCT_REQUIREMENT"));
            if (UtilValidate.isNotEmpty(facilityId)) {
                purchReqConds.add(EntityCondition.makeCondition("facilityId", facilityId));
            }
            long purchases = EntityQuery.use(delegator).from("Requirement").where(purchReqConds).queryCount();

            Map<String, Object> mrpProposals = new LinkedHashMap<>();
            mrpProposals.put("productionRuns", productionRuns);
            mrpProposals.put("purchases", purchases);

            // lastMrpRun
            GenericValue mrpRun = EntityQuery.use(delegator).from("MrpRun").orderBy("-startDate").queryFirst();
            Map<String, Object> lastMrpRun = null;
            if (mrpRun != null) {
                lastMrpRun = new LinkedHashMap<>();
                lastMrpRun.put("mrpId", mrpRun.getString("mrpId"));
                lastMrpRun.put("mrpName", mrpRun.getString("mrpName"));
                lastMrpRun.put("facilityId", mrpRun.getString("facilityId"));
                lastMrpRun.put("statusId", mrpRun.getString("statusId"));
                lastMrpRun.put("startDate", mrpRun.getTimestamp("startDate"));
                lastMrpRun.put("finishDate", mrpRun.getTimestamp("finishDate"));
                lastMrpRun.put("eventCount", mrpRun.getLong("eventCount"));
                lastMrpRun.put("proposedProductionRuns", mrpRun.getLong("proposedProductionRuns"));
                lastMrpRun.put("proposedPurchases", mrpRun.getLong("proposedPurchases"));
                lastMrpRun.put("errorCount", mrpRun.getLong("errorCount"));
            }

            // workCenterLoad
            Timestamp todayStart = UtilDateTime.getDayStart(now);
            Map<String, Object> wclCtx = new LinkedHashMap<>();
            if (UtilValidate.isNotEmpty(facilityId)) {
                wclCtx.put("facilityId", facilityId);
            }
            wclCtx.put("fromDate", todayStart);
            wclCtx.put("thruDate", UtilDateTime.addDaysToTimestamp(todayStart, days));
            wclCtx.put("userLogin", userLogin);
            Map<String, Object> wclResult = dispatcher.runSync("getWorkCenterLoad", wclCtx);
            if (ServiceUtil.isError(wclResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(wclResult));
            }
            List<Map<String, Object>> workCenterLoad = UtilGenerics.cast(wclResult.get("workCenters"));

            Map<String, Object> result = ServiceUtil.returnSuccess();
            result.put("runCounts", runCounts);
            result.put("lateRuns", lateRuns);
            result.put("runningTasks", runningTasks);
            result.put("upcomingRuns", upcomingRuns);
            result.put("shortages", shortages);
            result.put("mrpProposals", mrpProposals);
            result.put("lastMrpRun", lastMrpRun);
            result.put("workCenterLoad", workCenterLoad);
            return result;
        } catch (GenericEntityException e) {
            Debug.logError(e, MODULE);
            return ServiceUtil.returnError(e.getMessage());
        } catch (GenericServiceException e) {
            Debug.logError(e, MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
    }

    /** Builds the common run summary map used by lateRuns and upcomingRuns. */
    private static Map<String, Object> buildRunSummary(Delegator delegator, GenericValue run, Timestamp now) throws GenericEntityException {
        Timestamp estimatedCompletionDate = run.getTimestamp("estimatedCompletionDate");
        long daysLate = estimatedCompletionDate != null
                ? (now.getTime() - estimatedCompletionDate.getTime()) / (24L * 60 * 60 * 1000)
                : 0L;
        Map<String, Object> runMap = new LinkedHashMap<>();
        runMap.put("workEffortId", run.getString("workEffortId"));
        runMap.put("workEffortName", run.getString("workEffortName"));
        runMap.put("productId", getRunProductId(delegator, run.getString("workEffortId")));
        runMap.put("currentStatusId", run.getString("currentStatusId"));
        runMap.put("estimatedCompletionDate", estimatedCompletionDate);
        runMap.put("quantityToProduce", run.getBigDecimal("quantityToProduce"));
        runMap.put("quantityProduced", run.getBigDecimal("quantityProduced"));
        runMap.put("daysLate", daysLate);
        return runMap;
    }

    /** Returns the deliverable productId of a production run, from its WorkEffortGoodStandard PRUN_PROD_DELIV record. */
    private static String getRunProductId(Delegator delegator, String workEffortId) throws GenericEntityException {
        GenericValue deliv = EntityQuery.use(delegator).from("WorkEffortGoodStandard")
                .where("workEffortId", workEffortId, "workEffortGoodStdTypeId", "PRUN_PROD_DELIV")
                .queryFirst();
        return deliv != null ? deliv.getString("productId") : null;
    }

    /** Resolves the TechDataCalendar for a work center: its own calendarId, else its parent's, else "DEFAULT". */
    private static GenericValue resolveCalendar(Delegator delegator, GenericValue workCenter) throws GenericEntityException {
        String calendarId = workCenter.getString("calendarId");
        GenericValue calendar = null;
        if (UtilValidate.isNotEmpty(calendarId)) {
            calendar = EntityQuery.use(delegator).from("TechDataCalendar").where("calendarId", calendarId).cache(true).queryOne();
        }
        if (calendar == null) {
            String parentFixedAssetId = workCenter.getString("parentFixedAssetId");
            if (UtilValidate.isNotEmpty(parentFixedAssetId)) {
                GenericValue parent = EntityQuery.use(delegator).from("FixedAsset").where("fixedAssetId", parentFixedAssetId).cache(true).queryOne();
                if (parent != null && UtilValidate.isNotEmpty(parent.getString("calendarId"))) {
                    calendar = EntityQuery.use(delegator).from("TechDataCalendar").where("calendarId", parent.getString("calendarId")).cache(true).queryOne();
                }
            }
        }
        if (calendar == null) {
            calendar = EntityQuery.use(delegator).from("TechDataCalendar").where("calendarId", "DEFAULT").cache(true).queryOne();
        }
        return calendar;
    }

    /** Computes a task's total load in minutes: setup plus per-unit time times the parent run's quantity to produce. */
    private static double computeTaskMinutes(GenericValue task, GenericValue parentRun) {
        Double setupMillis = task.getDouble("estimatedSetupMillis");
        Double unitMillis = task.getDouble("estimatedMilliSeconds");
        double setup = setupMillis != null ? setupMillis : 0.0;
        double unit = unitMillis != null ? unitMillis : 0.0;
        BigDecimal quantityToProduce = parentRun != null ? parentRun.getBigDecimal("quantityToProduce") : null;
        double totalMillis;
        if (quantityToProduce != null && quantityToProduce.signum() > 0) {
            totalMillis = setup + (unit * quantityToProduce.doubleValue());
        } else {
            totalMillis = setup + unit;
        }
        return totalMillis / 60000.0;
    }

    /** Spreads a task's total load minutes over the given days, in proportion to each day's overlap with [start, end). */
    private static Map<Timestamp, Double> spreadLoad(Timestamp start, Timestamp end, double totalMinutes, List<Timestamp> days) {
        Map<Timestamp, Double> spread = new LinkedHashMap<>();
        if (start == null || end == null) {
            return spread;
        }
        long totalSpan = end.getTime() - start.getTime();
        if (totalSpan <= 0) {
            for (Timestamp day : days) {
                Timestamp dayEnd = UtilDateTime.addDaysToTimestamp(day, 1);
                if (!start.before(day) && start.before(dayEnd)) {
                    spread.put(day, totalMinutes);
                    break;
                }
            }
            return spread;
        }
        for (Timestamp day : days) {
            Timestamp dayEnd = UtilDateTime.addDaysToTimestamp(day, 1);
            long overlapStart = Math.max(day.getTime(), start.getTime());
            long overlapEnd = Math.min(dayEnd.getTime(), end.getTime());
            long overlap = overlapEnd - overlapStart;
            if (overlap > 0) {
                double portion = (double) overlap / (double) totalSpan;
                spread.put(day, totalMinutes * portion);
            }
        }
        return spread;
    }

}

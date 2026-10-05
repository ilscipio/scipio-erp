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
package com.ilscipio.scipio.manufacturing.mcp;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.sql.Date;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityQuery;

import com.ilscipio.scipio.mcp.catalog.ResultConverter;
import com.ilscipio.scipio.mcp.def.McpParam;
import com.ilscipio.scipio.mcp.def.McpResource;
import com.ilscipio.scipio.mcp.def.McpServer;
import com.ilscipio.scipio.mcp.def.McpServiceTool;
import com.ilscipio.scipio.mcp.def.McpTool;
import com.ilscipio.scipio.mcp.def.McpTopic;
import com.ilscipio.scipio.mcp.protocol.JsonRpc;
import com.ilscipio.scipio.mcp.registry.McpCallContext;
import com.ilscipio.scipio.mcp.registry.McpToolException;
import com.ilscipio.scipio.mcp.tool.ImportDiff;

/**
 * SCIPIO: 4.0.0: MCP server profile for the manufacturing component: BOMs, routings and production runs.
 */
@McpServer(name = "manufacturing", title = "Scipio Manufacturing", component = "manufacturing",
        description = "Manufacturing: bills of material, production runs, fabrication orders, MRP and shop floor.",
        featuredServices = {"createProductionRun", "updateProductionRun", "changeProductionRunStatus",
                "createBOMAssoc", "updateProductManufacturingRule", "createMrpEvent", "getProductRouting", "getManufacturingComponents",
                "quickRunAllProductionRunTasks", "executeMrp", "getWorkCenterLoad", "getManufacturingDashboard", "getShopFloorTasks",
                "declareProductionRunTaskReject", "getProductionRunRejects", "getProductStandardCost", "getProductWhereUsed",
                "createProductionRunsForOrder", "changeProductionRunTaskStatus", "updateProductionRunTask"},
        entities = {"ProductManufacturingRule", "TechDataCalendar", "TechDataCalendarExcDay", "WorkEffortGoodStandard",
                "TechDataCalendarExcWeek", "TechDataCalendarWeek", "MrpEventType", "MrpEvent", "MrpEventView", "MrpRun", "ProductionRunReject"},
        serviceTools = {
            @McpServiceTool(service = "updateProductionRunTask",
                    topic = "production_run",
                    name = "task_update",
                    description = "Update a production run task's fields.",
                    readOnly = false,
                    destructive = "false",
                    order = 1000),
            @McpServiceTool(service = "declareProductionRunTaskReject",
                    topic = "production_run",
                    name = "task_reject",
                    description = "Record a reject (scrap) quantity and reason for a production run task.",
                    readOnly = false,
                    destructive = "false",
                    order = 1010),
            @McpServiceTool(service = "quickRunAllProductionRunTasks",
                    topic = "production_run",
                    name = "task_quick_run_all",
                    description = "Quickly run and complete every task of a production run in sequence.",
                    readOnly = false,
                    destructive = "true",
                    requiresConfirmation = true,
                    order = 1020),
            @McpServiceTool(service = "createProductionRun", topic = "production_run", name = "create",
                    description = "Create a production run for a product and quantity.", readOnly = false, destructive = "false", requiresConfirmation = true, order = 40),
            @McpServiceTool(service = "changeProductionRunStatus", topic = "production_run", name = "set_status",
                    description = "Move a production run to the next status or a given one.", readOnly = false, destructive = "true", requiresConfirmation = true, order = 50),
            @McpServiceTool(service = "createProductionRunsForOrder", topic = "production_run", name = "create_for_order",
                    description = "Create production runs for the manufactured items of a sales order.", readOnly = false, destructive = "false", requiresConfirmation = true, order = 45),
            @McpServiceTool(service = "changeProductionRunTaskStatus", topic = "production_run", name = "task_set_status",
                    description = "Start or complete one task of a running production run.", readOnly = false, destructive = "true", requiresConfirmation = true, order = 55,
                    permission = "MANUFACTURING_FLOOR"),
            @McpServiceTool(service = "getManufacturingDashboard", topic = "shop_floor", name = "dashboard",
                    description = "Show run counts, late runs, shortages and MRP proposals.", readOnly = true, order = 60),
            @McpServiceTool(service = "getWorkCenterLoad", topic = "shop_floor", name = "work_center_load",
                    description = "Compare capacity to planned load per work center and day.", readOnly = true, order = 61),
            @McpServiceTool(service = "getShopFloorTasks", topic = "shop_floor", name = "tasks",
                    description = "List tasks ready to start, declare or complete on the shop floor.", readOnly = true, order = 62, permission = "MANUFACTURING_FLOOR"),
            @McpServiceTool(service = "getProductionRunRejects", topic = "production_run", name = "rejects",
                    description = "List reject records of a production run or task, with reasons.", readOnly = true, order = 63),
            @McpServiceTool(service = "getProductStandardCost", topic = "bom", name = "cost_get",
                    description = "Get a product's standard cost and its BOM component costs.", readOnly = true, order = 70),
            @McpServiceTool(service = "getProductWhereUsed", topic = "bom", name = "where_used",
                    description = "List every assembly that uses a product, at any depth.", readOnly = true, order = 71),
            @McpServiceTool(service = "executeMrp", topic = "mrp", name = "run",
                    description = "Run MRP for a facility or facility group.", readOnly = false, destructive = "true", requiresConfirmation = true, order = 80),
            @McpServiceTool(service = "getBOMTree", topic = "bom", name = "tree",
                    description = "Get the bill of material as a tree, components or where-used.", readOnly = true, order = 15),
            @McpServiceTool(service = "getFabricationOrder", topic = "fabrication_order", name = "get",
                    description = "Get a fabrication order header, its production runs and totals.", readOnly = true, order = 16),
            @McpServiceTool(service = "getProductionRunReservations", topic = "production_run", name = "lot_reservations",
                    description = "List open lot reservations of a production run's tasks.", readOnly = true, order = 17),
            @McpServiceTool(service = "getProductionRunScans", topic = "production_run", name = "scans_get",
                    description = "List recent barcode or QR scans for a production run or task.", readOnly = true, order = 18),
            @McpServiceTool(service = "createBOMAssoc", topic = "bom", name = "set",
                    description = "Add one bill-of-material component line to a product.", readOnly = false, destructive = "false", order = 41,
                    fixed = {"productAssocTypeId=MANUF_COMPONENT"}),
            @McpServiceTool(service = "updateProductAssoc", topic = "bom", name = "update",
                    description = "Update one bill-of-material component line.", readOnly = false, destructive = "false", order = 42,
                    fixed = {"productAssocTypeId=MANUF_COMPONENT"}),
            @McpServiceTool(service = "deleteProductAssoc", topic = "bom", name = "remove",
                    description = "Remove one bill-of-material component line.", readOnly = false, destructive = "true", requiresConfirmation = true, order = 72,
                    fixed = {"productAssocTypeId=MANUF_COMPONENT"}),
            @McpServiceTool(service = "createFabricationOrder", topic = "fabrication_order", name = "create",
                    description = "Create a fabrication order header.", readOnly = false, destructive = "false", order = 43),
            @McpServiceTool(service = "updateFabricationOrder", topic = "fabrication_order", name = "update",
                    description = "Update a fabrication order header.", readOnly = false, destructive = "false", order = 44),
            @McpServiceTool(service = "changeFabricationOrderStatus", topic = "fabrication_order", name = "set_status",
                    description = "Move every open production run of a fabrication order to a status.", readOnly = false, destructive = "true", requiresConfirmation = true, order = 73),
            @McpServiceTool(service = "addProductionRunToFabricationOrder", topic = "fabrication_order", name = "run_add",
                    description = "Add an existing production run to a fabrication order.", readOnly = false, destructive = "false", order = 46),
            @McpServiceTool(service = "removeProductionRunFromFabricationOrder", topic = "fabrication_order", name = "run_remove",
                    description = "Remove a production run from its fabrication order.", readOnly = false, destructive = "false", order = 47),
            @McpServiceTool(service = "reserveProductionRunLot", topic = "production_run", name = "lot_reserve",
                    description = "Hold a whole inventory lot for a production run task.", readOnly = false, destructive = "true", requiresConfirmation = true, order = 74),
            @McpServiceTool(service = "releaseProductionRunLot", topic = "production_run", name = "lot_release",
                    description = "Release a lot reservation back to available-to-promise.", readOnly = false, destructive = "true", requiresConfirmation = true, order = 75),
            @McpServiceTool(service = "recordProductionRunScan", topic = "production_run", name = "scan",
                    description = "Record one barcode or QR scan for a production run task.", readOnly = false, destructive = "true", order = 56, permission = "MANUFACTURING_FLOOR"),
            @McpServiceTool(service = "quickChangeProductionRunStatus", topic = "production_run", name = "release",
                    description = "Release a created run to scheduled status.", readOnly = false, destructive = "true", requiresConfirmation = true, order = 51,
                    fixed = {"statusId=PRUN_SCHEDULED"}),
            @McpServiceTool(service = "cancelProductionRun", topic = "production_run", name = "cancel",
                    description = "Cancel a production run.", readOnly = false, destructive = "true", requiresConfirmation = true, order = 76),
            @McpServiceTool(service = "addProductionRunComponent", topic = "production_run", name = "component_add",
                    description = "Add a component product to an existing production run.", readOnly = false, destructive = "false", order = 64),
            @McpServiceTool(service = "updateProductionRunComponent", topic = "production_run", name = "component_update",
                    description = "Update a component of an existing production run.", readOnly = false, destructive = "false", order = 65),
            @McpServiceTool(service = "replaceProductionRunComponent", topic = "production_run", name = "component_replace",
                    description = "Replace a production run task's component with a different product.", readOnly = false, destructive = "false", order = 66),
            @McpServiceTool(service = "switchProductionRunTaskMachine", topic = "production_run", name = "task_switch_machine",
                    description = "Switch the machine or line assigned to a production run task.", readOnly = false, destructive = "false", order = 67),
            @McpServiceTool(service = "productionRunProduce", topic = "production_run", name = "produce",
                    description = "Create inventory for the product a production run produced.", readOnly = false, destructive = "true", requiresConfirmation = true, order = 77),
            @McpServiceTool(service = "issueProductionRunTask", topic = "production_run", name = "issue_components",
                    description = "Issue the planned inventory for a production run task.", readOnly = false, destructive = "true", requiresConfirmation = true, order = 78),
            @McpServiceTool(service = "updateRequirement", topic = "mrp", name = "proposal_approve",
                    description = "Approve an MRP-proposed requirement.", readOnly = false, destructive = "true", requiresConfirmation = true, order = 83,
                    fixed = {"statusId=REQ_APPROVED"}),
            @McpServiceTool(service = "updateRequirement", topic = "mrp", name = "proposal_reject",
                    description = "Reject an MRP-proposed requirement.", readOnly = false, destructive = "true", requiresConfirmation = true, order = 84,
                    fixed = {"statusId=REQ_REJECTED"}),
            @McpServiceTool(service = "createProductionRunFromRequirement", topic = "mrp", name = "requirement_to_run",
                    description = "Create a production run from an approved internal requirement.", readOnly = false, destructive = "false", requiresConfirmation = true, order = 85),
            @McpServiceTool(service = "createFixedAsset", topic = "shop_floor", name = "work_center_create",
                    description = "Create a work center.", readOnly = false, destructive = "false", order = 48,
                    fixed = {"fixedAssetTypeId=PRODUCTION_EQUIPMENT"}),
            @McpServiceTool(service = "createCalendar", topic = "shop_floor", name = "calendar_create",
                    description = "Create a work calendar.", readOnly = false, destructive = "false", order = 68),
            @McpServiceTool(service = "createCalendarWeek", topic = "shop_floor", name = "calendar_week_set",
                    description = "Create a calendar week of open days and capacity.", readOnly = false, destructive = "false", order = 69),
            @McpServiceTool(service = "addProductManufacturingRule", topic = "bom", name = "rule_add",
                    description = "Add a product manufacturing substitution rule.", readOnly = false, destructive = "false", order = 49),
            @McpServiceTool(service = "calculateProductCosts", topic = "bom", name = "cost_calculate",
                    description = "Recalculate a product's standard costs from its cost components or BOM.", readOnly = false, destructive = "true", requiresConfirmation = true, order = 86)
        },
        topics = {
            @McpTopic(name = "bom", title = "Bills of Material", order = 10, featured = true,
                    description = "Bill of material: explode, edit, import, routings, costs."),
            @McpTopic(name = "production_run", title = "Production Runs", order = 20, featured = true,
                    description = "Production runs: find, create, run tasks, components, lots, scans."),
            @McpTopic(name = "fabrication_order", title = "Fabrication Orders", order = 30,
                    description = "Fabrication orders: get, create, update, status, production runs."),
            @McpTopic(name = "mrp", title = "MRP", order = 40,
                    description = "MRP: run, find, proposals, approve, reject, convert to run."),
            @McpTopic(name = "shop_floor", title = "Shop Floor", order = 50,
                    description = "Shop floor: dashboard, tasks, work centers, calendars, scheduling.")
        })
public final class ManufacturingMcp {

    private ManufacturingMcp() {}

    @McpTool(topic = "bom", name = "get", description = "Explode a product's bill of material into components and quantities.", readOnly = true, order = 10)
    public static Object getBom(McpCallContext ctx,
            @McpParam(name = "productId", required = true) String productId,
            @McpParam(name = "quantity", description = "Quantity to build, default 1", required = false) BigDecimal quantity) throws McpToolException {
        Map<String, Object> params = new LinkedHashMap<>();
        params.put("productId", productId);
        params.put("quantity", quantity != null ? quantity : BigDecimal.ONE);
        Map<String, Object> res = ctx.runService("getManufacturingComponents", params);
        Map<String, Object> out = new LinkedHashMap<>();
        out.put("productId", productId);
        out.put("quantity", quantity != null ? quantity : BigDecimal.ONE);
        out.put("components", ResultConverter.toJson(res.get("componentsMap")));
        return out;
    }

    @McpTool(topic = "production_run", name = "find", description = "Find production runs by id, status, facility or produced product.", readOnly = true, order = 20)
    public static Object findProductionRuns(McpCallContext ctx,
            @McpParam(name = "productionRunId", required = false) String productionRunId,
            @McpParam(name = "currentStatusId", description = "e.g. PRUN_CREATED, PRUN_SCHEDULED, PRUN_RUNNING, PRUN_COMPLETED", required = false) String currentStatusId,
            @McpParam(name = "facilityId", required = false) String facilityId,
            @McpParam(name = "productId", description = "Product the run produces", required = false) String productId,
            @McpParam(name = "limit", required = false) Integer limit) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        try {
            List<EntityCondition> conds = new ArrayList<>();
            conds.add(EntityCondition.makeCondition("workEffortTypeId", "PROD_ORDER_HEADER"));
            if (productionRunId != null) conds.add(EntityCondition.makeCondition("workEffortId", productionRunId));
            if (currentStatusId != null) conds.add(EntityCondition.makeCondition("currentStatusId", currentStatusId));
            if (facilityId != null) conds.add(EntityCondition.makeCondition("facilityId", facilityId));
            if (productId != null) {
                List<String> ids = new ArrayList<>();
                for (GenericValue g : EntityQuery.use(delegator).from("WorkEffortGoodStandard")
                        .where("productId", productId, "workEffortGoodStdTypeId", "PRUN_PROD_DELIV").queryList()) {
                    ids.add(g.getString("workEffortId"));
                }
                if (ids.isEmpty()) return new ArrayList<>();
                conds.add(EntityCondition.makeCondition("workEffortId", EntityOperator.IN, ids));
            }
            List<Map<String, Object>> out = new ArrayList<>();
            for (GenericValue we : EntityQuery.use(delegator).from("WorkEffort").where(conds).orderBy("-estimatedStartDate").maxRows(ctx.limit(limit)).queryList()) {
                Map<String, Object> row = new LinkedHashMap<>();
                row.put("productionRunId", we.getString("workEffortId"));
                row.put("workEffortName", we.getString("workEffortName"));
                row.put("currentStatusId", we.getString("currentStatusId"));
                row.put("facilityId", we.getString("facilityId"));
                row.put("quantityToProduce", ResultConverter.toJson(we.getBigDecimal("quantityToProduce")));
                row.put("quantityProduced", ResultConverter.toJson(we.getBigDecimal("quantityProduced")));
                row.put("estimatedStartDate", ResultConverter.toJson(we.getTimestamp("estimatedStartDate")));
                row.put("estimatedCompletionDate", ResultConverter.toJson(we.getTimestamp("estimatedCompletionDate")));
                GenericValue good = EntityQuery.use(delegator).from("WorkEffortGoodStandard")
                        .where("workEffortId", we.getString("workEffortId"), "workEffortGoodStdTypeId", "PRUN_PROD_DELIV").queryFirst();
                row.put("productId", good != null ? good.getString("productId") : null);
                out.add(row);
            }
            return out;
        } catch (GenericEntityException e) {
            throw new McpToolException("Production run search failed: " + e.getMessage());
        }
    }

    @McpTool(topic = "production_run", name = "declare", description = "Declare produced and rejected quantity, setup and run minutes for a task.", readOnly = false, destructive = "true", requiresConfirmation = true, order = 52,
            permission = "MANUFACTURING_FLOOR")
    public static Object declareTask(McpCallContext ctx,
            @McpParam(name = "productionRunId", required = true) String productionRunId,
            @McpParam(name = "workEffortId", description = "Task id", required = true) String workEffortId,
            @McpParam(name = "quantityProduced", required = false) BigDecimal quantityProduced,
            @McpParam(name = "quantityRejected", required = false) BigDecimal quantityRejected,
            @McpParam(name = "reasonEnumId", description = "Reject reason: PRUN_REJ_MATERIAL, PRUN_REJ_MACHINE, PRUN_REJ_OPERATOR, PRUN_REJ_INSPECTION, PRUN_REJ_OTHER", required = false) String reasonEnumId,
            @McpParam(name = "setupMinutes", required = false) Long setupMinutes,
            @McpParam(name = "taskMinutes", required = false) Long taskMinutes,
            @McpParam(name = "comments", required = false) String comments,
            @McpParam(name = "backflush", description = "Issue planned components in proportion to the produced quantity", required = false) Boolean backflush) throws McpToolException {
        Map<String, Object> params = new LinkedHashMap<>();
        params.put("productionRunId", productionRunId);
        params.put("productionRunTaskId", workEffortId);
        if (quantityProduced != null) params.put("addQuantityProduced", quantityProduced);
        if (setupMinutes != null) params.put("addSetupTime", Long.valueOf(setupMinutes.longValue() * 60000L));
        if (taskMinutes != null) params.put("addTaskTime", Long.valueOf(taskMinutes.longValue() * 60000L));
        if (comments != null) params.put("comments", comments);
        if (backflush != null) params.put("issueRequiredComponents", backflush);
        Map<String, Object> out = new LinkedHashMap<>();
        out.put("update", ResultConverter.toJson(ctx.runService("updateProductionRunTask", params)));
        if (quantityRejected != null && quantityRejected.signum() > 0) {
            if (reasonEnumId == null) throw new McpToolException("reasonEnumId is required when quantityRejected is set");
            Map<String, Object> rejectParams = new LinkedHashMap<>();
            rejectParams.put("productionRunId", productionRunId);
            rejectParams.put("workEffortId", workEffortId);
            rejectParams.put("quantity", quantityRejected);
            rejectParams.put("reasonEnumId", reasonEnumId);
            if (comments != null) rejectParams.put("comments", comments);
            out.put("reject", ResultConverter.toJson(ctx.runService("declareProductionRunTaskReject", rejectParams)));
        }
        return out;
    }

    @McpTool(topic = "mrp", name = "find", description = "List MRP runs, newest first, with their counts.", readOnly = true, order = 81)
    public static Object findMrpRuns(McpCallContext ctx,
            @McpParam(name = "facilityId", required = false) String facilityId,
            @McpParam(name = "statusId", required = false) String statusId,
            @McpParam(name = "limit", required = false) Integer limit) throws McpToolException {
        try {
            List<EntityCondition> conds = new ArrayList<>();
            if (facilityId != null) conds.add(EntityCondition.makeCondition("facilityId", facilityId));
            if (statusId != null) conds.add(EntityCondition.makeCondition("statusId", statusId));
            return ResultConverter.toJson(EntityQuery.use(ctx.getDelegator()).from("MrpRun").where(conds)
                    .orderBy("-startDate").maxRows(ctx.limit(limit)).queryList());
        } catch (GenericEntityException e) {
            throw new McpToolException("MRP run search failed: " + e.getMessage());
        }
    }

    @McpTool(topic = "mrp", name = "proposals", description = "List MRP-proposed production and purchase requirements.", readOnly = true, order = 82)
    public static Object findMrpProposals(McpCallContext ctx,
            @McpParam(name = "facilityId", required = false) String facilityId,
            @McpParam(name = "requirementTypeId", description = "INTERNAL_REQUIREMENT or PRODUCT_REQUIREMENT", required = false) String requirementTypeId,
            @McpParam(name = "limit", required = false) Integer limit) throws McpToolException {
        try {
            List<EntityCondition> conds = new ArrayList<>();
            conds.add(EntityCondition.makeCondition("statusId", "REQ_PROPOSED"));
            if (facilityId != null) conds.add(EntityCondition.makeCondition("facilityId", facilityId));
            if (requirementTypeId != null) {
                conds.add(EntityCondition.makeCondition("requirementTypeId", requirementTypeId));
            } else {
                conds.add(EntityCondition.makeCondition("requirementTypeId", EntityOperator.IN, java.util.Arrays.asList("INTERNAL_REQUIREMENT", "PRODUCT_REQUIREMENT")));
            }
            return ResultConverter.toJson(EntityQuery.use(ctx.getDelegator()).from("Requirement").where(conds)
                    .orderBy("requirementStartDate").maxRows(ctx.limit(limit)).queryList());
        } catch (GenericEntityException e) {
            throw new McpToolException("Requirement search failed: " + e.getMessage());
        }
    }

    @McpTool(topic = "production_run", name = "get", description = "Get one production run: header, tasks, produced and consumed products.", readOnly = true, order = 30)
    public static Object getProductionRun(McpCallContext ctx,
            @McpParam(name = "productionRunId", required = true) String productionRunId) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        try {
            GenericValue we = EntityQuery.use(delegator).from("WorkEffort").where("workEffortId", productionRunId).queryOne();
            if (we == null) throw new McpToolException("Production run not found: " + productionRunId);
            Map<String, Object> out = new LinkedHashMap<>();
            out.put("header", ResultConverter.toJson(we));
            out.put("tasks", ResultConverter.toJson(EntityQuery.use(delegator).from("WorkEffort")
                    .where("workEffortParentId", productionRunId).orderBy("priority", "workEffortId").queryList()));
            out.put("goods", ResultConverter.toJson(EntityQuery.use(delegator).from("WorkEffortGoodStandard").where("workEffortId", productionRunId).queryList()));
            return out;
        } catch (GenericEntityException e) {
            throw new McpToolException("Failed to load production run " + productionRunId + ": " + e.getMessage());
        }
    }

    @McpTool(topic = "bom", name = "routing_create", description = "Create a routing with its tasks.", readOnly = false, destructive = "false", order = 90)
    public static Object createRouting(McpCallContext ctx,
            @McpParam(name = "routingName", required = true) String routingName,
            @McpParam(name = "routingId", required = false) String routingId,
            @McpParam(name = "productId", description = "Product to link the routing to via ROU_PROD_TEMPLATE", required = false) String productId,
            @McpParam(name = "quantityToProduce", required = false) BigDecimal quantityToProduce,
            @McpParam(name = "tasks", description = "Array of {name, fixedAssetId, setupMinutes, runMinutes, sequence, description}", required = false, type = "array") List<Object> tasks) throws McpToolException {
        Map<String, Object> routingParams = new LinkedHashMap<>();
        if (routingId != null) routingParams.put("workEffortId", routingId);
        routingParams.put("workEffortTypeId", "ROUTING");
        routingParams.put("currentStatusId", "ROU_ACTIVE");
        routingParams.put("workEffortName", routingName);
        Map<String, Object> routingResult = ctx.runService("createWorkEffort", routingParams);
        String newRoutingId = (String) routingResult.get("workEffortId");
        if (newRoutingId == null) newRoutingId = routingId;

        List<Map<String, Object>> outTasks = new ArrayList<>();
        if (tasks != null) {
            int seq = 10;
            for (Object o : tasks) {
                if (!(o instanceof Map)) continue;
                Map<?, ?> taskRow = (Map<?, ?>) o;
                String name = str(taskRow.get("name"));
                String fixedAssetId = str(taskRow.get("fixedAssetId"));
                Long setupMinutes = toLong(taskRow.get("setupMinutes"));
                Long runMinutes = toLong(taskRow.get("runMinutes"));
                Integer sequence = toInt(taskRow.get("sequence"));
                String description = str(taskRow.get("description"));
                Map<String, Object> taskParams = new LinkedHashMap<>();
                taskParams.put("workEffortTypeId", "ROU_TASK");
                taskParams.put("currentStatusId", "ROU_ACTIVE");
                taskParams.put("workEffortName", name);
                if (fixedAssetId != null) taskParams.put("fixedAssetId", fixedAssetId);
                if (setupMinutes != null) taskParams.put("estimatedSetupMillis", Double.valueOf(setupMinutes * 60000L));
                if (runMinutes != null) taskParams.put("estimatedMilliSeconds", Double.valueOf(runMinutes * 60000L));
                if (description != null) taskParams.put("description", description);
                Map<String, Object> taskResult = ctx.runService("createWorkEffort", taskParams);
                String taskId = (String) taskResult.get("workEffortId");
                int seqNum = sequence != null ? sequence : seq;

                Map<String, Object> assocParams = new LinkedHashMap<>();
                assocParams.put("workEffortIdFrom", newRoutingId);
                assocParams.put("workEffortIdTo", taskId);
                assocParams.put("workEffortAssocTypeId", "ROUTING_COMPONENT");
                assocParams.put("sequenceNum", Long.valueOf(seqNum));
                assocParams.put("fromDate", UtilDateTime.nowTimestamp());
                ctx.runService("createWorkEffortAssoc", assocParams);

                Map<String, Object> outTask = new LinkedHashMap<>();
                outTask.put("taskId", taskId);
                outTask.put("sequence", seqNum);
                outTask.put("name", name);
                outTasks.add(outTask);
                seq += 10;
            }
        }
        boolean productLinked = false;
        if (productId != null) {
            Map<String, Object> goodParams = new LinkedHashMap<>();
            goodParams.put("productId", productId);
            goodParams.put("workEffortId", newRoutingId);
            goodParams.put("workEffortGoodStdTypeId", "ROU_PROD_TEMPLATE");
            goodParams.put("statusId", "WEGS_CREATED");
            goodParams.put("fromDate", UtilDateTime.nowTimestamp());
            if (quantityToProduce != null) goodParams.put("estimatedQuantity", quantityToProduce);
            ctx.runService("createWorkEffortGoodStandard", goodParams);
            productLinked = true;
        }
        Map<String, Object> out = new LinkedHashMap<>();
        out.put("routingId", newRoutingId);
        out.put("tasks", outTasks);
        out.put("productLinked", productLinked);
        return out;
    }

    @McpTool(topic = "bom", name = "routing_task_add", description = "Add one task to an existing routing.", readOnly = false, destructive = "false", order = 91)
    public static Object addRoutingTask(McpCallContext ctx,
            @McpParam(name = "routingId", required = true) String routingId,
            @McpParam(name = "name", required = true) String name,
            @McpParam(name = "fixedAssetId", required = false) String fixedAssetId,
            @McpParam(name = "setupMinutes", required = false) Long setupMinutes,
            @McpParam(name = "runMinutes", required = false) Long runMinutes,
            @McpParam(name = "sequence", required = false) Integer sequence) throws McpToolException {
        Map<String, Object> taskParams = new LinkedHashMap<>();
        taskParams.put("workEffortTypeId", "ROU_TASK");
        taskParams.put("currentStatusId", "ROU_ACTIVE");
        taskParams.put("workEffortName", name);
        if (fixedAssetId != null) taskParams.put("fixedAssetId", fixedAssetId);
        if (setupMinutes != null) taskParams.put("estimatedSetupMillis", Double.valueOf(setupMinutes * 60000L));
        if (runMinutes != null) taskParams.put("estimatedMilliSeconds", Double.valueOf(runMinutes * 60000L));
        Map<String, Object> taskResult = ctx.runService("createWorkEffort", taskParams);
        String taskId = (String) taskResult.get("workEffortId");

        Map<String, Object> assocParams = new LinkedHashMap<>();
        assocParams.put("workEffortIdFrom", routingId);
        assocParams.put("workEffortIdTo", taskId);
        assocParams.put("workEffortAssocTypeId", "ROUTING_COMPONENT");
        assocParams.put("sequenceNum", Long.valueOf(sequence != null ? sequence.longValue() : 10L));
        assocParams.put("fromDate", UtilDateTime.nowTimestamp());
        ctx.runService("createWorkEffortAssoc", assocParams);

        Map<String, Object> out = new LinkedHashMap<>();
        out.put("taskId", taskId);
        out.put("routingId", routingId);
        return out;
    }

    @McpTool(topic = "bom", name = "routing_get", description = "Get a routing's header and tasks with minutes and work center.", readOnly = true, order = 21)
    public static Object getRouting(McpCallContext ctx,
            @McpParam(name = "productId", required = false) String productId,
            @McpParam(name = "routingId", description = "workEffortId of the routing", required = false) String routingId) throws McpToolException {
        if (productId == null && routingId == null) throw new McpToolException("productId or routingId is required");
        Map<String, Object> params = new LinkedHashMap<>();
        if (productId != null) params.put("productId", productId);
        if (routingId != null) params.put("workEffortId", routingId);
        Map<String, Object> res = ctx.runService("getProductRouting", params);
        Object routing = res.get("routing");
        String rid = routing instanceof GenericValue ? ((GenericValue) routing).getString("workEffortId") : routingId;
        Map<String, Object> out = new LinkedHashMap<>();
        out.put("routing", ResultConverter.toJson(routing));
        List<Map<String, Object>> tasks = new ArrayList<>();
        if (res.get("tasks") instanceof List) {
            for (Object o : (List<?>) res.get("tasks")) {
                if (!(o instanceof GenericValue)) continue;
                GenericValue assoc = (GenericValue) o;
                Map<String, Object> row = new LinkedHashMap<>();
                row.put("workEffortId", assoc.getString("workEffortIdTo"));
                row.put("sequenceNum", assoc.get("sequenceNum"));
                GenericValue task = routingTask(ctx, assoc.getString("workEffortIdTo"));
                if (task != null) {
                    row.put("workEffortName", task.getString("workEffortName"));
                    row.put("fixedAssetId", task.getString("fixedAssetId"));
                    row.put("estimatedSetupMillis", task.get("estimatedSetupMillis"));
                    row.put("estimatedMilliSeconds", task.get("estimatedMilliSeconds"));
                    row.put("description", task.getString("description"));
                }
                tasks.add(row);
            }
        }
        out.put("tasks", tasks);
        if (rid != null) {
            Map<String, Object> assocParams = new LinkedHashMap<>();
            assocParams.put("workEffortId", rid);
            Map<String, Object> assocRes = ctx.runService("getRoutingTaskAssocs", assocParams);
            out.put("routingTaskAssocs", ResultConverter.toJson(assocRes.get("routingTaskAssocs")));
        }
        return out;
    }

    /** The task WorkEffort behind one routing WorkEffortAssoc row; getProductRouting returns the assoc rows. */
    private static GenericValue routingTask(McpCallContext ctx, String taskId) throws McpToolException {
        try {
            return EntityQuery.use(ctx.getDelegator()).from("WorkEffort").where("workEffortId", taskId).queryOne();
        } catch (GenericEntityException e) {
            throw new McpToolException("Routing task lookup failed: " + e.getMessage());
        }
    }

    @McpTool(topic = "shop_floor", name = "schedule_check", description = "Check whether a quantity can be produced by a due date.", readOnly = true, order = 22)
    public static Object checkSchedule(McpCallContext ctx,
            @McpParam(name = "productId", required = true) String productId,
            @McpParam(name = "quantity", required = true) BigDecimal quantity,
            @McpParam(name = "dueDate", description = "yyyy-MM-dd", required = true) String dueDate,
            @McpParam(name = "facilityId", required = true) String facilityId,
            @McpParam(name = "routingId", required = false) String routingId) throws McpToolException {
        Timestamp dueTs;
        try {
            dueTs = new Timestamp(Date.valueOf(dueDate).getTime());
        } catch (IllegalArgumentException e) {
            throw new McpToolException("dueDate must be formatted yyyy-MM-dd");
        }
        Timestamp today = UtilDateTime.getDayStart(UtilDateTime.nowTimestamp());
        if (!dueTs.after(today)) dueTs = UtilDateTime.addDaysToTimestamp(today, 1);

        Map<String, Object> routingParams = new LinkedHashMap<>();
        routingParams.put("productId", productId);
        if (routingId != null) routingParams.put("workEffortId", routingId);
        Map<String, Object> routingRes = ctx.runService("getProductRouting", routingParams);
        Object routingObj = routingRes.get("routing");
        List<?> taskList = routingObj != null ? (List<?>) routingRes.get("tasks") : null;
        if (routingObj == null || taskList == null || taskList.isEmpty()) {
            Map<String, Object> out = new LinkedHashMap<>();
            out.put("feasible", null);
            out.put("reason", "No routing found for product " + productId);
            return out;
        }
        String resolvedRoutingId = ((GenericValue) routingObj).getString("workEffortId");

        Map<String, Double> requiredMinutesByWorkCenter = new LinkedHashMap<>();
        List<Map<String, Object>> taskDetails = new ArrayList<>();
        for (Object o : taskList) {
            if (!(o instanceof GenericValue)) continue;
            String taskId = ((GenericValue) o).getString("workEffortIdTo");
            GenericValue task = routingTask(ctx, taskId);
            if (task == null) continue;
            String fixedAssetId = task.getString("fixedAssetId");
            Map<String, Object> estParams = new LinkedHashMap<>();
            estParams.put("taskId", taskId);
            estParams.put("productId", productId);
            estParams.put("routingId", resolvedRoutingId);
            estParams.put("quantity", quantity);
            Map<String, Object> estRes = ctx.runService("getEstimatedTaskTime", estParams);
            Object estimatedTaskTime = estRes.get("estimatedTaskTime");
            double minutes = toDouble(estimatedTaskTime) / 60000.0;
            if (fixedAssetId != null) {
                requiredMinutesByWorkCenter.merge(fixedAssetId, minutes, Double::sum);
            }
            Map<String, Object> taskDetail = new LinkedHashMap<>();
            taskDetail.put("taskId", taskId);
            taskDetail.put("workEffortName", task.getString("workEffortName"));
            taskDetail.put("fixedAssetId", fixedAssetId);
            taskDetail.put("requiredMinutes", minutes);
            taskDetails.add(taskDetail);
        }

        if (requiredMinutesByWorkCenter.isEmpty()) {
            Map<String, Object> out = new LinkedHashMap<>();
            out.put("feasible", null);
            out.put("reason", "Routing tasks have no assigned work centers");
            out.put("tasks", taskDetails);
            return out;
        }

        Map<String, Object> loadParams = new LinkedHashMap<>();
        loadParams.put("facilityId", facilityId);
        loadParams.put("fromDate", today);
        loadParams.put("thruDate", UtilDateTime.addDaysToTimestamp(dueTs, 1));
        Map<String, Object> loadRes = ctx.runService("getWorkCenterLoad", loadParams);
        List<?> loadRows = (List<?>) loadRes.get("loadRows");
        if (loadRows == null || loadRes.get("days") == null) {
            Map<String, Object> out = new LinkedHashMap<>();
            out.put("feasible", null);
            out.put("reason", "No capacity data available for facility " + facilityId);
            out.put("tasks", taskDetails);
            return out;
        }

        boolean feasible = true;
        Timestamp latestCompletion = today;
        Map<String, Object> bottleneck = null;
        for (Map.Entry<String, Double> entry : requiredMinutesByWorkCenter.entrySet()) {
            String fixedAssetId = entry.getKey();
            double requiredMinutes = entry.getValue();
            List<Map<?, ?>> wcRows = new ArrayList<>();
            for (Object rowObj : loadRows) {
                if (!(rowObj instanceof Map)) continue;
                Map<?, ?> row = (Map<?, ?>) rowObj;
                if (fixedAssetId.equals(row.get("fixedAssetId"))) wcRows.add(row);
            }
            if (wcRows.isEmpty()) {
                feasible = false;
                if (bottleneck == null) bottleneck = bottleneckMap(fixedAssetId, requiredMinutes, 0.0);
                continue;
            }
            double cumulative = 0.0;
            double totalFree = 0.0;
            Timestamp reachedOn = null;
            for (Map<?, ?> row : wcRows) {
                double free = Math.max(0.0, toDouble(row.get("capacityMinutes")) - toDouble(row.get("loadMinutes")));
                totalFree += free;
                cumulative += free;
                if (reachedOn == null && cumulative >= requiredMinutes) reachedOn = (Timestamp) row.get("day");
            }
            if (reachedOn == null) {
                feasible = false;
                if (bottleneck == null || requiredMinutes > toDouble(bottleneck.get("requiredMinutes"))) {
                    bottleneck = bottleneckMap(fixedAssetId, requiredMinutes, totalFree);
                }
            } else {
                if (reachedOn.after(latestCompletion)) latestCompletion = reachedOn;
                if (reachedOn.after(dueTs)) {
                    feasible = false;
                    if (bottleneck == null) bottleneck = bottleneckMap(fixedAssetId, requiredMinutes, totalFree);
                }
            }
        }

        Map<String, Object> out = new LinkedHashMap<>();
        out.put("feasible", feasible);
        out.put("earliestCompletionDate", ResultConverter.toJson(latestCompletion));
        out.put("bottleneck", bottleneck);
        out.put("tasks", taskDetails);
        return out;
    }

    private static Map<String, Object> bottleneckMap(String fixedAssetId, double requiredMinutes, double freeMinutes) {
        Map<String, Object> b = new LinkedHashMap<>();
        b.put("fixedAssetId", fixedAssetId);
        b.put("requiredMinutes", requiredMinutes);
        b.put("freeMinutes", freeMinutes);
        return b;
    }

    @McpTool(topic = "production_run", name = "cost_variance", description = "Compare a production run's actual cost to its planned standard cost.", readOnly = true, order = 23)
    public static Object runCostVariance(McpCallContext ctx,
            @McpParam(name = "productionRunId", required = true) String productionRunId) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        GenericValue we;
        String producedProductId;
        try {
            we = EntityQuery.use(delegator).from("WorkEffort").where("workEffortId", productionRunId).queryOne();
            if (we == null) throw new McpToolException("Production run not found: " + productionRunId);
            GenericValue good = EntityQuery.use(delegator).from("WorkEffortGoodStandard")
                    .where("workEffortId", productionRunId, "workEffortGoodStdTypeId", "PRUN_PROD_DELIV").queryFirst();
            producedProductId = good != null ? good.getString("productId") : null;
        } catch (GenericEntityException e) {
            throw new McpToolException("Failed to load production run " + productionRunId + ": " + e.getMessage());
        }
        if (producedProductId == null) throw new McpToolException("Production run " + productionRunId + " has no produced product on record");

        BigDecimal quantity = we.getBigDecimal("quantityProduced");
        if (quantity == null || quantity.signum() == 0) quantity = we.getBigDecimal("quantityToProduce");
        if (quantity == null || quantity.signum() == 0) quantity = BigDecimal.ONE;

        Map<String, Object> actual = new LinkedHashMap<>();
        BigDecimal actualTotal;
        Map<String, Object> costParams = new LinkedHashMap<>();
        costParams.put("workEffortId", productionRunId);
        try {
            Map<String, Object> costRes = ctx.runService("getWorkEffortCosts", costParams);
            actual.put("byType", ResultConverter.toJson(costRes.get("costComponents")));
            actualTotal = (BigDecimal) costRes.get("totalCost");
        } catch (McpToolException e) {
            Map<String, Object> costRes = ctx.runService("getProductionRunCost", costParams);
            actualTotal = (BigDecimal) costRes.get("totalCost");
        }
        actual.put("total", ResultConverter.toJson(actualTotal));

        Map<String, Object> plannedParams = new LinkedHashMap<>();
        plannedParams.put("productId", producedProductId);
        Map<String, Object> plannedRes = ctx.runService("getProductStandardCost", plannedParams);
        BigDecimal quantityFinal = quantity;
        Map<String, Object> planned = new LinkedHashMap<>();
        planned.put("material", ResultConverter.toJson(mul((BigDecimal) plannedRes.get("materialCost"), quantityFinal)));
        planned.put("labor", ResultConverter.toJson(mul((BigDecimal) plannedRes.get("laborCost"), quantityFinal)));
        planned.put("overhead", ResultConverter.toJson(mul((BigDecimal) plannedRes.get("overheadCost"), quantityFinal)));
        planned.put("routing", ResultConverter.toJson(mul((BigDecimal) plannedRes.get("routingCost"), quantityFinal)));
        BigDecimal plannedTotal = mul((BigDecimal) plannedRes.get("totalCost"), quantityFinal);
        planned.put("total", ResultConverter.toJson(plannedTotal));

        BigDecimal actualTotalSafe = actualTotal != null ? actualTotal : BigDecimal.ZERO;
        BigDecimal plannedTotalSafe = plannedTotal != null ? plannedTotal : BigDecimal.ZERO;
        BigDecimal varianceTotal = actualTotalSafe.subtract(plannedTotalSafe);
        Map<String, Object> variance = new LinkedHashMap<>();
        variance.put("total", ResultConverter.toJson(varianceTotal));
        variance.put("perUnit", ResultConverter.toJson(quantityFinal.signum() != 0
                ? varianceTotal.divide(quantityFinal, 4, RoundingMode.HALF_UP) : BigDecimal.ZERO));
        variance.put("percent", ResultConverter.toJson(plannedTotalSafe.signum() != 0
                ? varianceTotal.divide(plannedTotalSafe, 4, RoundingMode.HALF_UP).multiply(BigDecimal.valueOf(100))
                : null));

        Map<String, Object> out = new LinkedHashMap<>();
        out.put("productionRunId", productionRunId);
        out.put("productId", producedProductId);
        out.put("quantity", ResultConverter.toJson(quantityFinal));
        out.put("planned", planned);
        out.put("actual", actual);
        out.put("variance", variance);
        return out;
    }

    @McpTool(topic = "bom", name = "import", description = "Import bill-of-material rows; creates or updates lines, dry run by default.", readOnly = false, destructive = "true", requiresConfirmation = true, order = 92)
    public static Object importBom(McpCallContext ctx,
            @McpParam(name = "rows", required = true, type = "array") List<Object> rows,
            @McpParam(name = "apply", required = false) Boolean apply,
            @McpParam(name = "force", required = false) Boolean force) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        ImportDiff diff = new ImportDiff(apply, force);
        List<Map<String, Object>> list = ImportDiff.rows(rows);
        List<Map<String, Object>> plans = new ArrayList<>();

        for (int i = 0; i < list.size(); i++) {
            Map<String, Object> row = list.get(i);
            String parentId = ImportDiff.str(row, "parent", "parentSku", "productId");
            String componentId = ImportDiff.str(row, "component", "componentSku", "productIdTo");
            BigDecimal quantity = ImportDiff.decimal(row, "quantity", "qty");
            String uom = ImportDiff.str(row, "uom", "quantityUomId");
            BigDecimal scrap = ImportDiff.decimal(row, "scrap", "scrapFactor");
            Integer sequence = ImportDiff.integer(row, "sequence", "sequenceNum");
            String name = ImportDiff.str(row, "name", "componentName");
            String supplierPartyId = ImportDiff.str(row, "supplier", "supplierPartyId");
            BigDecimal price = ImportDiff.decimal(row, "supplierPrice", "price");
            Integer leadDays = ImportDiff.integer(row, "leadDays");

            if (parentId == null) {
                diff.doubt(i, "row has no parent product id");
                continue;
            }

            boolean componentIsNew = false;
            String resolvedComponentId = componentId;
            if (resolvedComponentId == null) {
                if (name == null) {
                    diff.doubt(i, "row has no component product id or name");
                    continue;
                }
                resolvedComponentId = ImportDiff.slug(name, 20);
                componentIsNew = true;
            } else {
                try {
                    if (EntityQuery.use(delegator).from("Product").where("productId", resolvedComponentId).queryOne() == null) {
                        if (name == null) {
                            diff.doubt(i, "component product " + resolvedComponentId + " not found and no name given");
                            continue;
                        }
                        componentIsNew = true;
                    }
                } catch (GenericEntityException e) {
                    throw new McpToolException("Product lookup failed: " + e.getMessage());
                }
            }
            if (componentIsNew) {
                Map<String, Object> productData = new LinkedHashMap<>();
                productData.put("productId", resolvedComponentId);
                productData.put("productTypeId", "RAW_MATERIAL");
                productData.put("internalName", name);
                productData.put("productName", name);
                diff.create("product", resolvedComponentId, productData);
            }

            String bomKey = parentId + "->" + resolvedComponentId;
            GenericValue existingAssoc = null;
            if (!componentIsNew) {
                try {
                    existingAssoc = EntityQuery.use(delegator).from("ProductAssoc")
                            .where("productId", parentId, "productIdTo", resolvedComponentId, "productAssocTypeId", "MANUF_COMPONENT")
                            .filterByDate().queryFirst();
                } catch (GenericEntityException e) {
                    throw new McpToolException("BOM lookup failed: " + e.getMessage());
                }
            }
            Map<String, Object> bomData = new LinkedHashMap<>();
            bomData.put("productId", parentId);
            if (quantity != null) bomData.put("quantity", quantity);
            if (uom != null) bomData.put("quantityUomId", uom);
            if (scrap != null) bomData.put("scrapFactor", scrap);
            if (sequence != null) bomData.put("sequenceNum", sequence);

            String bomAction;
            if (existingAssoc == null) {
                diff.create("bom", bomKey, bomData);
                bomAction = "create";
            } else {
                BigDecimal existingQty = existingAssoc.getBigDecimal("quantity");
                if (quantity != null && (existingQty == null || quantity.compareTo(existingQty) != 0)) {
                    Map<String, Object> before = new LinkedHashMap<>();
                    before.put("quantity", existingQty);
                    Map<String, Object> after = new LinkedHashMap<>();
                    after.put("quantity", quantity);
                    diff.update("bom", bomKey, before, after);
                    bomAction = "update";
                } else {
                    diff.unchanged("bom", bomKey);
                    bomAction = "unchanged";
                }
            }

            boolean supplierDoubt = false;
            if (supplierPartyId != null) {
                try {
                    if (EntityQuery.use(delegator).from("PartyGroup").where("partyId", supplierPartyId).queryOne() == null) {
                        diff.doubt(i, "supplier party " + supplierPartyId + " not found");
                        supplierDoubt = true;
                    }
                } catch (GenericEntityException e) {
                    throw new McpToolException("Supplier lookup failed: " + e.getMessage());
                }
            }

            Map<String, Object> plan = new LinkedHashMap<>();
            plan.put("rowIndex", i);
            plan.put("parentId", parentId);
            plan.put("componentId", resolvedComponentId);
            plan.put("componentIsNew", componentIsNew);
            plan.put("name", name);
            plan.put("bomAction", bomAction);
            plan.put("bomData", bomData);
            plan.put("supplierPartyId", supplierDoubt ? null : supplierPartyId);
            plan.put("price", price);
            plan.put("leadDays", leadDays);
            plans.add(plan);
        }

        if (diff.canWrite()) {
            for (Map<String, Object> plan : plans) {
                String parentId = (String) plan.get("parentId");
                String componentId = (String) plan.get("componentId");
                boolean componentIsNew = Boolean.TRUE.equals(plan.get("componentIsNew"));
                @SuppressWarnings("unchecked")
                Map<String, Object> bomData = (Map<String, Object>) plan.get("bomData");
                String bomAction = (String) plan.get("bomAction");
                String supplierPartyId = (String) plan.get("supplierPartyId");
                BigDecimal price = (BigDecimal) plan.get("price");
                Integer leadDays = (Integer) plan.get("leadDays");

                if (componentIsNew) {
                    Map<String, Object> productParams = new LinkedHashMap<>();
                    productParams.put("productId", componentId);
                    productParams.put("productTypeId", "RAW_MATERIAL");
                    productParams.put("internalName", plan.get("name"));
                    productParams.put("productName", plan.get("name"));
                    Map<String, Object> productResult = ctx.runService("createProduct", productParams);
                    Object createdId = productResult.get("productId");
                    if (createdId != null) componentId = (String) createdId;
                }

                Map<String, Object> bomParams = new LinkedHashMap<>(bomData);
                bomParams.put("productIdTo", componentId);
                bomParams.put("productAssocTypeId", "MANUF_COMPONENT");
                if ("create".equals(bomAction)) {
                    ctx.runService("createBOMAssoc", bomParams);
                } else if ("update".equals(bomAction)) {
                    ctx.runService("updateProductAssoc", bomParams);
                }

                if (supplierPartyId != null && price != null) {
                    try {
                        long existingCount = EntityQuery.use(delegator).from("SupplierProduct")
                                .where("productId", componentId, "partyId", supplierPartyId).queryCount();
                        if (existingCount == 0) {
                            Map<String, Object> supplierParams = new LinkedHashMap<>();
                            supplierParams.put("supplierProductId", delegator.getNextSeqId("SupplierProduct"));
                            supplierParams.put("productId", componentId);
                            supplierParams.put("partyId", supplierPartyId);
                            supplierParams.put("currencyUomId", ctx.getCurrencyUomId() != null ? ctx.getCurrencyUomId() : "USD");
                            supplierParams.put("minimumOrderQuantity", BigDecimal.ONE);
                            supplierParams.put("availableFromDate", UtilDateTime.nowTimestamp());
                            supplierParams.put("lastPrice", price);
                            if (leadDays != null) supplierParams.put("standardLeadTimeDays", leadDays);
                            ctx.runService("createSupplierProduct", supplierParams);
                        }
                    } catch (GenericEntityException e) {
                        throw new McpToolException("Supplier product lookup failed: " + e.getMessage());
                    }
                }

                Map<String, Object> writtenIds = new LinkedHashMap<>();
                writtenIds.put("componentId", componentId);
                diff.written("bom", parentId + "->" + componentId, writtenIds);
            }
        }
        return diff.result();
    }

    @McpResource(uri = "scipio://production-run/{productionRunId}", name = "Production Run",
            description = "Production run header, tasks, produced/consumed goods, task components and barcode scans.",
            mimeType = "application/json")
    public static String productionRunResource(McpCallContext ctx, Map<String, String> uriParams) throws McpToolException {
        String productionRunId = uriParams.get("productionRunId");
        Delegator delegator = ctx.getDelegator();
        try {
            GenericValue we = EntityQuery.use(delegator).from("WorkEffort").where("workEffortId", productionRunId).queryOne();
            if (we == null) throw new McpToolException("Production run not found: " + productionRunId);
            List<GenericValue> tasks = EntityQuery.use(delegator).from("WorkEffort")
                    .where("workEffortParentId", productionRunId).orderBy("priority", "workEffortId").queryList();
            List<String> taskIds = new ArrayList<>();
            for (GenericValue t : tasks) taskIds.add(t.getString("workEffortId"));

            Map<String, Object> map = new LinkedHashMap<>();
            map.put("header", ResultConverter.toJson(we));
            map.put("tasks", ResultConverter.toJson(tasks));
            map.put("goods", ResultConverter.toJson(EntityQuery.use(delegator).from("WorkEffortGoodStandard").where("workEffortId", productionRunId).queryList()));
            map.put("components", taskIds.isEmpty() ? ResultConverter.toJson(new ArrayList<>())
                    : ResultConverter.toJson(EntityQuery.use(delegator).from("WorkEffortGoodStandard")
                            .where(EntityCondition.makeCondition("workEffortId", EntityOperator.IN, taskIds)).queryList()));
            Map<String, Object> scanParams = new LinkedHashMap<>();
            scanParams.put("productionRunId", productionRunId);
            Map<String, Object> scanRes = ctx.runService("getProductionRunScans", scanParams);
            map.put("scans", ResultConverter.toJson(scanRes.get("scans")));
            return JsonRpc.writePretty(map);
        } catch (GenericEntityException e) {
            throw new McpToolException("Failed to load production run resource " + productionRunId + ": " + e.getMessage());
        }
    }

    private static BigDecimal mul(BigDecimal a, BigDecimal b) {
        if (a == null || b == null) return null;
        return a.multiply(b);
    }

    private static String str(Object o) {
        return o != null ? String.valueOf(o) : null;
    }

    private static Long toLong(Object o) {
        if (o == null) return null;
        if (o instanceof Number) return ((Number) o).longValue();
        return Long.parseLong(o.toString());
    }

    private static Integer toInt(Object o) {
        if (o == null) return null;
        if (o instanceof Number) return ((Number) o).intValue();
        return Integer.parseInt(o.toString());
    }

    private static double toDouble(Object o) {
        if (o == null) return 0.0;
        if (o instanceof Number) return ((Number) o).doubleValue();
        try {
            return Double.parseDouble(o.toString());
        } catch (NumberFormatException e) {
            return 0.0;
        }
    }
}

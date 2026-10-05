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
package com.ilscipio.scipio.workeffort.mcp;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityQuery;

import com.ilscipio.scipio.mcp.catalog.ResultConverter;
import com.ilscipio.scipio.mcp.def.McpParam;
import com.ilscipio.scipio.mcp.def.McpServer;
import com.ilscipio.scipio.mcp.def.McpServiceTool;
import com.ilscipio.scipio.mcp.def.McpTool;
import com.ilscipio.scipio.mcp.def.McpTopic;
import com.ilscipio.scipio.mcp.registry.McpCallContext;
import com.ilscipio.scipio.mcp.registry.McpToolException;

/**
 * SCIPIO: 4.0.0: MCP server profile for the workeffort component: tasks, events, timesheets and deliverables.
 */
@McpServer(name = "workeffort", title = "Scipio Work Effort", component = "workeffort",
        description = "Work effort: tasks, events, assignments and timesheets.",
        featuredServices = {"createWorkEffort", "updateWorkEffort", "assignPartyToWorkEffort", "createTimesheet",
                "createTimeEntry", "createDeliverable", "createWorkEffortNote"},
        entities = {"WorkEffort", "WorkEffortAssoc", "WorkEffortAttribute", "WorkEffortPartyAssignment", "Timesheet", "TimeEntry",
                "TimesheetRole", "Deliverable", "DeliverableType"},
        serviceTools = {
            @McpServiceTool(service = "createWorkEffort", topic = "task", name = "create",
                    description = "Create a task or event.", readOnly = false, destructive = "false", order = 30, exclude = {"quickAssignPartyId"}),
            @McpServiceTool(service = "updateWorkEffort", topic = "task", name = "update",
                    description = "Update a task or event's name, dates or description.", readOnly = false, order = 40),
            @McpServiceTool(service = "assignPartyToWorkEffort", topic = "task", name = "assign",
                    description = "Assign a party to a task or event with a role.", readOnly = false, destructive = "false", order = 50),
            @McpServiceTool(service = "createWorkEffortNote", topic = "task", name = "note_create",
                    description = "Add a note to a task or event.", readOnly = false, destructive = "false", order = 42),
            @McpServiceTool(service = "updateWorkEffortNote", topic = "task", name = "note_update",
                    description = "Update a note on a task or event.", readOnly = false, destructive = "false", order = 49),
            @McpServiceTool(service = "createTimesheetForThisWeek", topic = "task", name = "timesheet_create",
                    description = "Create a timesheet for the current week.", readOnly = false, destructive = "false", order = 45),
            @McpServiceTool(service = "createTimeEntry", topic = "task", name = "time_entry_create",
                    description = "Log a time entry.", readOnly = false, destructive = "false", order = 46),
            @McpServiceTool(service = "updateTimeEntry", topic = "task", name = "time_entry_update",
                    description = "Update a logged time entry.", readOnly = false, destructive = "false", order = 47),
            @McpServiceTool(service = "updatePartyToWorkEffortAssignment", topic = "task", name = "assignment_update",
                    description = "Update a party assignment on a task or event.", readOnly = false, destructive = "false", order = 56),
            @McpServiceTool(service = "createWorkEffortAssoc", topic = "task", name = "assoc_add",
                    description = "Link two work efforts.", readOnly = false, destructive = "false", order = 60),
            @McpServiceTool(service = "createWorkEffortEventReminder", topic = "task", name = "event_reminder_add",
                    description = "Add a reminder to a task or event.", readOnly = false, destructive = "false", order = 48),
            @McpServiceTool(service = "createWorkEffortContent", topic = "task", name = "content_attach",
                    description = "Attach content to a task or event.", readOnly = false, destructive = "false", order = 52),
            @McpServiceTool(service = "createWorkEffortRequest", topic = "task", name = "request_create",
                    description = "Link a customer request to a task or event.", readOnly = false, destructive = "false", order = 53),
            @McpServiceTool(service = "updateWorkEffort", topic = "task", name = "set_status",
                    description = "Set a task or event's status.", readOnly = false, destructive = "true", requiresConfirmation = true, order = 70),
            @McpServiceTool(service = "addTimesheetToInvoice", topic = "task", name = "timesheet_to_invoice",
                    description = "Add a timesheet's hours to an invoice.", readOnly = false, destructive = "true", requiresConfirmation = true, order = 75),
            @McpServiceTool(service = "updateTimesheet", topic = "task", name = "timesheet_set_status",
                    description = "Set a timesheet's status.", readOnly = false, destructive = "true", requiresConfirmation = true, order = 80)
        },
        topics = {
            @McpTopic(name = "task", title = "Tasks", order = 10, featured = true,
                    description = "Tasks: find, create, assign, update status, log time.")
        })
public final class WorkEffortMcp {

    private WorkEffortMcp() {}

    @McpTool(topic = "task", name = "find", description = "Find tasks or events by type, status, party or date.", readOnly = true, order = 10)
    public static Object findWorkEfforts(McpCallContext ctx,
            @McpParam(name = "workEffortId", required = false) String workEffortId,
            @McpParam(name = "workEffortTypeId", description = "e.g. TASK, EVENT, PROJECT, MILESTONE", required = false) String workEffortTypeId,
            @McpParam(name = "currentStatusId", description = "e.g. CAL_NEEDS_ACTION, CAL_ACCEPTED, CAL_COMPLETED", required = false) String currentStatusId,
            @McpParam(name = "partyId", description = "Assigned party", required = false) String partyId,
            @McpParam(name = "nameLike", description = "Partial name match", required = false) String nameLike,
            @McpParam(name = "fromDate", description = "Estimated start on/after", required = false) Timestamp fromDate,
            @McpParam(name = "thruDate", description = "Estimated start on/before", required = false) Timestamp thruDate,
            @McpParam(name = "limit", required = false) Integer limit) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        try {
            List<EntityCondition> conds = new ArrayList<>();
            if (workEffortId != null) conds.add(EntityCondition.makeCondition("workEffortId", workEffortId));
            if (workEffortTypeId != null) conds.add(EntityCondition.makeCondition("workEffortTypeId", workEffortTypeId));
            if (currentStatusId != null) conds.add(EntityCondition.makeCondition("currentStatusId", currentStatusId));
            if (nameLike != null) conds.add(EntityCondition.makeCondition("workEffortName", EntityOperator.LIKE, "%" + nameLike + "%"));
            if (fromDate != null) conds.add(EntityCondition.makeCondition("estimatedStartDate", EntityOperator.GREATER_THAN_EQUAL_TO, fromDate));
            if (thruDate != null) conds.add(EntityCondition.makeCondition("estimatedStartDate", EntityOperator.LESS_THAN_EQUAL_TO, thruDate));
            if (partyId != null) {
                List<String> ids = new ArrayList<>();
                for (GenericValue a : EntityQuery.use(delegator).from("WorkEffortPartyAssignment").where("partyId", partyId).filterByDate().queryList()) {
                    ids.add(a.getString("workEffortId"));
                }
                if (ids.isEmpty()) return new ArrayList<>();
                conds.add(EntityCondition.makeCondition("workEffortId", EntityOperator.IN, ids));
            }
            List<GenericValue> rows = EntityQuery.use(delegator).from("WorkEffort").where(conds)
                    .orderBy("-estimatedStartDate", "-createdStamp").maxRows(ctx.limit(limit)).queryList();
            List<Map<String, Object>> out = new ArrayList<>();
            for (GenericValue we : rows) {
                Map<String, Object> row = new LinkedHashMap<>();
                for (String f : new String[] {"workEffortId", "workEffortTypeId", "currentStatusId", "workEffortName", "priority", "workEffortParentId"}) {
                    row.put(f, ResultConverter.toJson(we.get(f)));
                }
                row.put("estimatedStartDate", ResultConverter.toJson(we.getTimestamp("estimatedStartDate")));
                row.put("estimatedCompletionDate", ResultConverter.toJson(we.getTimestamp("estimatedCompletionDate")));
                out.add(row);
            }
            return out;
        } catch (GenericEntityException e) {
            throw new McpToolException("Work effort search failed: " + e.getMessage());
        }
    }

    @McpTool(topic = "task", name = "get", description = "Get a task with assignments, children and attributes.", readOnly = true, order = 20)
    public static Object getWorkEffort(McpCallContext ctx,
            @McpParam(name = "workEffortId", required = true) String workEffortId) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        try {
            GenericValue we = EntityQuery.use(delegator).from("WorkEffort").where("workEffortId", workEffortId).queryOne();
            if (we == null) throw new McpToolException("Work effort not found: " + workEffortId);
            Map<String, Object> out = new LinkedHashMap<>();
            out.put("workEffort", ResultConverter.toJson(we));
            out.put("assignments", ResultConverter.toJson(EntityQuery.use(delegator).from("WorkEffortPartyAssignment").where("workEffortId", workEffortId).queryList()));
            out.put("children", ResultConverter.toJson(EntityQuery.use(delegator).from("WorkEffort").where("workEffortParentId", workEffortId).queryList()));
            out.put("associations", ResultConverter.toJson(EntityQuery.use(delegator).from("WorkEffortAssoc").where("workEffortIdFrom", workEffortId).queryList()));
            out.put("attributes", ResultConverter.toJson(EntityQuery.use(delegator).from("WorkEffortAttribute").where("workEffortId", workEffortId).queryList()));
            return out;
        } catch (GenericEntityException e) {
            throw new McpToolException("Failed to load work effort " + workEffortId + ": " + e.getMessage());
        }
    }
}

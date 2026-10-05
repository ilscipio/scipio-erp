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
package com.ilscipio.scipio.humanres.mcp;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

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
 * SCIPIO: 4.0.0: MCP server profile for the humanres component: employees, employment, positions, leave and reviews.
 */
@McpServer(name = "humanres", title = "Scipio Human Resources", component = "humanres",
        description = "Human resources: employees, positions, leave and hiring.",
        featuredServices = {"createEmployment", "updateEmployment", "createEmplPosition", "createPerfReview",
                "createEmplLeave", "createEmployee", "updateEmplLeaveStatus"},
        entities = {"Employment", "EmplPosition", "EmplPositionFulfillment", "EmplLeave", "PerfReview", "PartySkill", "BenefitType",
                "EmploymentApp", "PartyQual"},
        serviceTools = {
            @McpServiceTool(service = "createEmployee", topic = "employee", name = "create",
                    description = "Create an employee with a person, role and employment.", readOnly = false, destructive = "false", order = 30),
            @McpServiceTool(service = "createEmplPosition", topic = "position", name = "create",
                    description = "Create an employee position for an organization.",
                    readOnly = false, destructive = "false", order = 50),
            @McpServiceTool(service = "createEmplLeave", topic = "employee", name = "leave_create",
                    description = "Create a leave request for an employee.", readOnly = false, destructive = "false", order = 60),
            @McpServiceTool(service = "createEmployment", topic = "employee", name = "employment_create",
                    description = "Create an employment linking an employer and employee.", readOnly = false, destructive = "false", order = 35),
            @McpServiceTool(service = "createEmplPositionFulfillment", topic = "position", name = "fulfill",
                    description = "Fill an employee position with a party.", readOnly = false, destructive = "false", order = 52),
            @McpServiceTool(service = "updateEmplPosition", topic = "position", name = "update",
                    description = "Update an employee position.",
                    readOnly = false, destructive = "false", order = 53),
            @McpServiceTool(service = "updateEmplLeave", topic = "employee", name = "leave_update",
                    description = "Update a leave request.",
                    readOnly = false, destructive = "false", order = 62),
            @McpServiceTool(service = "updateEmplLeave", topic = "employee", name = "leave_approve",
                    description = "Approve a leave request.",
                    readOnly = false, destructive = "true", requiresConfirmation = true,
                    fixed = {"leaveStatus=LEAVE_APPROVED"}, order = 82),
            @McpServiceTool(service = "createPayHistory", topic = "employee", name = "pay_history_add",
                    description = "Add a pay history record for an employment.",
                    readOnly = false, destructive = "true", requiresConfirmation = true, order = 84),
            @McpServiceTool(service = "createPartySkill", topic = "employee", name = "skill_add",
                    description = "Add a skill to a party.",
                    readOnly = false, destructive = "false", order = 44),
            @McpServiceTool(service = "createPartyQual", topic = "employee", name = "qualification_add",
                    description = "Add a qualification to a party.",
                    readOnly = false, destructive = "false", order = 45),
            @McpServiceTool(service = "createPerfReview", topic = "employee", name = "perf_review_create",
                    description = "Create a performance review for an employee.",
                    readOnly = false, destructive = "false", order = 46),
            @McpServiceTool(service = "assignTraining", topic = "employee", name = "training_assign",
                    description = "Assign training to a person.",
                    readOnly = false, destructive = "false", order = 47),
            @McpServiceTool(service = "createJobRequisition", topic = "position", name = "job_requisition_create",
                    description = "Create a job requisition.",
                    readOnly = false, destructive = "false", order = 48),
            @McpServiceTool(service = "createEmploymentApp", topic = "position", name = "employment_app_create",
                    description = "Create an employment application.",
                    readOnly = false, destructive = "false", order = 49),
            @McpServiceTool(service = "updateEmployment", topic = "employee", name = "employment_end",
                    description = "End an employment with a termination reason.",
                    readOnly = false, destructive = "true", requiresConfirmation = true, order = 80)
        },
        topics = {
            @McpTopic(name = "employee", title = "Employees", order = 10, featured = true,
                    description = "Employees: find, hire, pay, train, review, request leave."),
            @McpTopic(name = "position", title = "Positions", order = 20,
                    description = "Positions: find, create, update, fill, requisition, apply.")
        })
public final class HumanResMcp {

    private HumanResMcp() {}

    @McpTool(topic = "employee", name = "find", description = "Find employees by name, party id or employer.", readOnly = true, order = 10)
    public static Object findEmployees(McpCallContext ctx,
            @McpParam(name = "partyId", required = false) String partyId,
            @McpParam(name = "firstName", description = "Partial match", required = false) String firstName,
            @McpParam(name = "lastName", description = "Partial match", required = false) String lastName,
            @McpParam(name = "employerPartyId", description = "Organization the employee works for (Employment.partyIdFrom)", required = false) String employerPartyId,
            @McpParam(name = "limit", required = false) Integer limit) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        int max = ctx.limit(limit);
        try {
            Set<String> employees = new LinkedHashSet<>();
            if (employerPartyId != null) {
                for (GenericValue e : EntityQuery.use(delegator).from("Employment").where("partyIdFrom", employerPartyId).filterByDate().queryList()) {
                    employees.add(e.getString("partyIdTo"));
                }
            } else {
                for (GenericValue r : EntityQuery.use(delegator).from("PartyRole").where("roleTypeId", "EMPLOYEE").queryList()) {
                    employees.add(r.getString("partyId"));
                }
            }
            if (partyId != null) employees.retainAll(java.util.Collections.singleton(partyId));
            if (employees.isEmpty()) return new ArrayList<>();
            List<EntityCondition> conds = new ArrayList<>();
            conds.add(EntityCondition.makeCondition("partyId", EntityOperator.IN, new ArrayList<>(employees)));
            if (firstName != null) conds.add(EntityCondition.makeCondition("firstName", EntityOperator.LIKE, "%" + firstName + "%"));
            if (lastName != null) conds.add(EntityCondition.makeCondition("lastName", EntityOperator.LIKE, "%" + lastName + "%"));
            List<Map<String, Object>> out = new ArrayList<>();
            for (GenericValue p : EntityQuery.use(delegator).from("Person").where(conds).orderBy("lastName", "firstName").maxRows(max).queryList()) {
                Map<String, Object> row = new LinkedHashMap<>();
                row.put("partyId", p.getString("partyId"));
                row.put("firstName", p.getString("firstName"));
                row.put("lastName", p.getString("lastName"));
                GenericValue emp = EntityQuery.use(delegator).from("Employment").where("partyIdTo", p.getString("partyId")).filterByDate().queryFirst();
                row.put("employerPartyId", emp != null ? emp.getString("partyIdFrom") : null);
                out.add(row);
            }
            return out;
        } catch (GenericEntityException e) {
            throw new McpToolException("Employee search failed: " + e.getMessage());
        }
    }

    @McpTool(topic = "employee", name = "get", description = "Get an employee with employments, skills and leave records.", readOnly = true, order = 20)
    public static Object getEmployee(McpCallContext ctx,
            @McpParam(name = "partyId", required = true) String partyId) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        try {
            GenericValue person = EntityQuery.use(delegator).from("Person").where("partyId", partyId).queryOne();
            if (person == null) throw new McpToolException("Person not found: " + partyId);
            Map<String, Object> out = new LinkedHashMap<>();
            out.put("person", ResultConverter.toJson(person));
            out.put("employments", ResultConverter.toJson(EntityQuery.use(delegator).from("Employment").where("partyIdTo", partyId).queryList()));
            out.put("positions", ResultConverter.toJson(EntityQuery.use(delegator).from("EmplPositionFulfillment").where("partyId", partyId).queryList()));
            out.put("skills", ResultConverter.toJson(EntityQuery.use(delegator).from("PartySkill").where("partyId", partyId).queryList()));
            out.put("leave", ResultConverter.toJson(EntityQuery.use(delegator).from("EmplLeave").where("partyId", partyId).orderBy("-fromDate").queryList()));
            return out;
        } catch (GenericEntityException e) {
            throw new McpToolException("Failed to load employee " + partyId + ": " + e.getMessage());
        }
    }

    @McpTool(topic = "position", name = "find", description = "Find employee positions by organization, type or status.", readOnly = true, order = 40)
    public static Object findPositions(McpCallContext ctx,
            @McpParam(name = "partyId", description = "Organization party id", required = false) String partyId,
            @McpParam(name = "emplPositionTypeId", required = false) String emplPositionTypeId,
            @McpParam(name = "statusId", description = "e.g. EMPL_POS_ACTIVE, EMPL_POS_INACTIVE", required = false) String statusId,
            @McpParam(name = "employeePartyId", description = "Employee currently filling the position", required = false) String employeePartyId,
            @McpParam(name = "limit", required = false) Integer limit) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        try {
            List<EntityCondition> conds = new ArrayList<>();
            if (partyId != null) conds.add(EntityCondition.makeCondition("partyId", partyId));
            if (emplPositionTypeId != null) conds.add(EntityCondition.makeCondition("emplPositionTypeId", emplPositionTypeId));
            if (statusId != null) conds.add(EntityCondition.makeCondition("statusId", statusId));
            if (employeePartyId != null) {
                List<String> ids = new ArrayList<>();
                for (GenericValue f : EntityQuery.use(delegator).from("EmplPositionFulfillment").where("partyId", employeePartyId).filterByDate().queryList()) {
                    ids.add(f.getString("emplPositionId"));
                }
                if (ids.isEmpty()) return new ArrayList<>();
                conds.add(EntityCondition.makeCondition("emplPositionId", EntityOperator.IN, ids));
            }
            return ResultConverter.toJson(EntityQuery.use(delegator).from("EmplPosition").where(conds).maxRows(ctx.limit(limit)).queryList());
        } catch (GenericEntityException e) {
            throw new McpToolException("Position search failed: " + e.getMessage());
        }
    }
}

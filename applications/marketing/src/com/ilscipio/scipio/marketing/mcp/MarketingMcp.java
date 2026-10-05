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
package com.ilscipio.scipio.marketing.mcp;

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
 * SCIPIO: 4.0.0: MCP server profile for the marketing component: campaigns, contact lists, segments and sales
 * force automation (leads, opportunities and forecasts). Covers both the marketing and sfa webapps.
 */
@McpServer(name = "marketing", title = "Scipio Marketing & SFA", component = "marketing",
        description = "Marketing and sales force automation: leads, opportunities, accounts, and campaigns.",
        featuredServices = {"createMarketingCampaign", "updateMarketingCampaign", "createContactList",
                "createTrackingCode", "createSegmentGroup", "createSalesForecast", "createSalesOpportunity", "createLead",
                "convertLeadToContact", "updateSalesOpportunity"},
        entities = {"MarketingCampaign", "MarketingCampaignRole", "ContactList", "ContactListParty", "SegmentGroup",
                "SegmentGroupRole", "TrackingCode", "TrackingCodeType", "SalesOpportunity", "SalesOpportunityRole", "SalesOpportunityStage"},
        serviceTools = {
            @McpServiceTool(service = "createLead", topic = "sfa", name = "lead_create",
                    description = "Create a lead: a person with optional company, email and phone.", readOnly = false, destructive = "false", order = 20),
            @McpServiceTool(service = "convertLeadToContact", topic = "sfa", name = "lead_convert",
                    description = "Convert a lead party into a contact or account.",
                    readOnly = false, requiresConfirmation = true, order = 25),
            @McpServiceTool(service = "createSalesOpportunity", topic = "sfa", name = "opportunity_create",
                    description = "Create a sales opportunity with amount, stage and close date.", readOnly = false, destructive = "false", order = 40),
            @McpServiceTool(service = "updateSalesOpportunity", topic = "sfa", name = "opportunity_update",
                    description = "Update a sales opportunity's stage, amount, probability or close date.", readOnly = false, order = 50),
            @McpServiceTool(service = "createMarketingCampaign", topic = "campaign", name = "create",
                    description = "Create a marketing campaign.", readOnly = false, destructive = "false", order = 70),
            @McpServiceTool(service = "createAccount", topic = "sfa", name = "account_create",
                    description = "Create an account (company) with address, phone and email.", readOnly = false, destructive = "false", order = 41),
            @McpServiceTool(service = "createContact", topic = "sfa", name = "contact_create",
                    description = "Create a contact person with address, phone and email.", readOnly = false, destructive = "false", order = 43),
            @McpServiceTool(service = "createWorkEffortAndPartyAssign", topic = "sfa", name = "activity_create",
                    description = "Create an activity (task, event or meeting) and assign it to a party.",
                    readOnly = false, destructive = "false", order = 45),
            @McpServiceTool(service = "createContactList", topic = "campaign", name = "contact_list_create",
                    description = "Create a contact list.",
                    readOnly = false, destructive = "false", order = 47),
            @McpServiceTool(service = "createContactListParty", topic = "campaign", name = "contact_list_party_add",
                    description = "Add a party to a contact list.",
                    readOnly = false, destructive = "false", order = 48),
            @McpServiceTool(service = "createTrackingCode", topic = "campaign", name = "tracking_code_create",
                    description = "Create a tracking code with type and redirect URLs.",
                    readOnly = false, destructive = "false", order = 49),
            @McpServiceTool(service = "createSegmentGroup", topic = "campaign", name = "segment_group_create",
                    description = "Create a segment group.",
                    readOnly = false, destructive = "false", order = 51),
            @McpServiceTool(service = "createSalesForecast", topic = "campaign", name = "sales_forecast_create",
                    description = "Create a sales forecast for the current user.",
                    readOnly = false, destructive = "false", order = 53),
            @McpServiceTool(service = "createCommunicationEvent", topic = "sfa", name = "comm_event_create",
                    description = "Create a communication event on a party.",
                    readOnly = false, destructive = "false", order = 55),
            @McpServiceTool(service = "updateMarketingCampaign", topic = "campaign", name = "update",
                    description = "Update a marketing campaign's name, dates, status or description.",
                    readOnly = false, order = 71),
            @McpServiceTool(service = "sendEmailToContactList", topic = "campaign", name = "contact_list_send",
                    description = "Send an email to every member of a contact list.",
                    readOnly = false, destructive = "true", requiresConfirmation = true, permission = "MCP_MAIL_SEND", order = 80),
            @McpServiceTool(service = "mergeContacts", topic = "sfa", name = "contacts_merge",
                    description = "Merge one party's contact details into another party.",
                    readOnly = false, destructive = "true", requiresConfirmation = true, order = 82)
        },
        topics = {
            @McpTopic(name = "sfa", title = "Sales force automation", order = 10, featured = true,
                    description = "Sales force automation: leads, opportunities, accounts, contacts, activities."),
            @McpTopic(name = "campaign", title = "Campaigns", order = 20,
                    description = "Marketing campaigns: create, update, tracking codes, lists, forecasts.")
        })
public final class MarketingMcp {

    private MarketingMcp() {}

    @McpTool(topic = "sfa", name = "lead_find", description = "Find leads (parties with the LEAD role) by name or company.", readOnly = true, order = 10)
    public static Object findLeads(McpCallContext ctx,
            @McpParam(name = "firstName", description = "Partial match", required = false) String firstName,
            @McpParam(name = "lastName", description = "Partial match", required = false) String lastName,
            @McpParam(name = "groupName", description = "Company name, partial match", required = false) String groupName,
            @McpParam(name = "limit", required = false) Integer limit) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        int max = ctx.limit(limit);
        try {
            Set<String> leads = new LinkedHashSet<>();
            for (GenericValue r : EntityQuery.use(delegator).from("PartyRole").where("roleTypeId", "LEAD").queryList()) {
                leads.add(r.getString("partyId"));
            }
            if (leads.isEmpty()) return new ArrayList<>();
            List<Map<String, Object>> out = new ArrayList<>();
            List<EntityCondition> conds = new ArrayList<>();
            conds.add(EntityCondition.makeCondition("partyId", EntityOperator.IN, new ArrayList<>(leads)));
            if (groupName != null) {
                conds.add(EntityCondition.makeCondition("groupName", EntityOperator.LIKE, "%" + groupName + "%"));
                for (GenericValue g : EntityQuery.use(delegator).from("PartyGroup").where(conds).maxRows(max).queryList()) {
                    Map<String, Object> row = new LinkedHashMap<>();
                    row.put("partyId", g.getString("partyId"));
                    row.put("groupName", g.getString("groupName"));
                    out.add(row);
                }
                return out;
            }
            if (firstName != null) conds.add(EntityCondition.makeCondition("firstName", EntityOperator.LIKE, "%" + firstName + "%"));
            if (lastName != null) conds.add(EntityCondition.makeCondition("lastName", EntityOperator.LIKE, "%" + lastName + "%"));
            for (GenericValue p : EntityQuery.use(delegator).from("Person").where(conds).orderBy("lastName").maxRows(max).queryList()) {
                Map<String, Object> row = new LinkedHashMap<>();
                row.put("partyId", p.getString("partyId"));
                row.put("firstName", p.getString("firstName"));
                row.put("lastName", p.getString("lastName"));
                out.add(row);
            }
            return out;
        } catch (GenericEntityException e) {
            throw new McpToolException("Lead search failed: " + e.getMessage());
        }
    }

    @McpTool(topic = "sfa", name = "opportunity_find", description = "Find sales opportunities by id, stage, name or related party.", readOnly = true, order = 30)
    public static Object findOpportunities(McpCallContext ctx,
            @McpParam(name = "salesOpportunityId", required = false) String salesOpportunityId,
            @McpParam(name = "opportunityStageId", description = "e.g. SOSTG_PROSPECT, SOSTG_QUALIFICATION, SOSTG_CLOSED_WON", required = false) String opportunityStageId,
            @McpParam(name = "nameLike", description = "Partial opportunity name match", required = false) String nameLike,
            @McpParam(name = "partyId", description = "Party in any opportunity role", required = false) String partyId,
            @McpParam(name = "limit", required = false) Integer limit) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        try {
            List<EntityCondition> conds = new ArrayList<>();
            if (salesOpportunityId != null) conds.add(EntityCondition.makeCondition("salesOpportunityId", salesOpportunityId));
            if (opportunityStageId != null) conds.add(EntityCondition.makeCondition("opportunityStageId", opportunityStageId));
            if (nameLike != null) conds.add(EntityCondition.makeCondition("opportunityName", EntityOperator.LIKE, "%" + nameLike + "%"));
            if (partyId != null) {
                List<String> ids = new ArrayList<>();
                for (GenericValue r : EntityQuery.use(delegator).from("SalesOpportunityRole").where("partyId", partyId).queryList()) {
                    ids.add(r.getString("salesOpportunityId"));
                }
                if (ids.isEmpty()) return new ArrayList<>();
                conds.add(EntityCondition.makeCondition("salesOpportunityId", EntityOperator.IN, ids));
            }
            return ResultConverter.toJson(EntityQuery.use(delegator).from("SalesOpportunity").where(conds)
                    .orderBy("-estimatedCloseDate").maxRows(ctx.limit(limit)).queryList());
        } catch (GenericEntityException e) {
            throw new McpToolException("Opportunity search failed: " + e.getMessage());
        }
    }

    @McpTool(topic = "campaign", name = "find", description = "Find marketing campaigns by id, status or name.", readOnly = true, order = 60)
    public static Object findCampaigns(McpCallContext ctx,
            @McpParam(name = "marketingCampaignId", required = false) String marketingCampaignId,
            @McpParam(name = "statusId", description = "e.g. MKTG_CAMP_PLANNED, MKTG_CAMP_INPROGRESS, MKTG_CAMP_COMPLETED", required = false) String statusId,
            @McpParam(name = "nameLike", description = "Partial campaign name match", required = false) String nameLike,
            @McpParam(name = "limit", required = false) Integer limit) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        try {
            List<EntityCondition> conds = new ArrayList<>();
            if (marketingCampaignId != null) conds.add(EntityCondition.makeCondition("marketingCampaignId", marketingCampaignId));
            if (statusId != null) conds.add(EntityCondition.makeCondition("statusId", statusId));
            if (nameLike != null) conds.add(EntityCondition.makeCondition("campaignName", EntityOperator.LIKE, "%" + nameLike + "%"));
            return ResultConverter.toJson(EntityQuery.use(delegator).from("MarketingCampaign").where(conds)
                    .orderBy("-fromDate").maxRows(ctx.limit(limit)).queryList());
        } catch (GenericEntityException e) {
            throw new McpToolException("Campaign search failed: " + e.getMessage());
        }
    }
}

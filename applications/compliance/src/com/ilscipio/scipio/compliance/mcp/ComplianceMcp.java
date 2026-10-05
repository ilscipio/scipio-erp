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
package com.ilscipio.scipio.compliance.mcp;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;

import com.ilscipio.scipio.compliance.LegalDocumentWorker;
import com.ilscipio.scipio.compliance.ThirdPartyServiceRegistry;
import com.ilscipio.scipio.mcp.def.McpParam;
import com.ilscipio.scipio.mcp.def.McpServer;
import com.ilscipio.scipio.mcp.def.McpServiceTool;
import com.ilscipio.scipio.mcp.def.McpTool;
import com.ilscipio.scipio.mcp.def.McpTopic;
import com.ilscipio.scipio.mcp.registry.McpCallContext;
import com.ilscipio.scipio.mcp.registry.McpToolException;

/**
 * SCIPIO: 4.0.0: MCP server profile of the compliance component: third-party services and legal texts of a store.
 */
@McpServer(name = "compliance", title = "Store compliance", component = "compliance",
        description = "Store compliance: third-party services that receive shopper data, legal texts, consent and privacy requests.",
        entities = {"StoreComplianceProfile", "LegalDocument", "ThirdPartyService", "ConsentEvent", "PrivacyRequest",
                "EprRegistration", "PackagingComponent", "MarketplaceSeller"},
        topics = {
            @McpTopic(name = "compliance", title = "Store compliance", order = 90, featured = true,
                    description = "Legal texts, third-party services and privacy settings of a store: list, check, publish.")
        },
        serviceTools = {
            @McpServiceTool(service = "publishLegalDocument", topic = "compliance", name = "publish_document",
                    description = "Publish a new version of a legal text (imprint, terms, privacy, cookies, withdrawal ...). "
                            + "Without bodyText the shipped template is published.",
                    readOnly = false, requiresConfirmation = true, order = 30),
            @McpServiceTool(service = "exportPartyPersonalData", topic = "compliance", name = "export_party",
                    description = "All personal data of a customer (party) as JSON, for an access or portability request.",
                    readOnly = true, requiresConfirmation = true, order = 60),
            @McpServiceTool(service = "anonymizePartyPersonalData", topic = "compliance", name = "anonymize_party",
                    description = "Delete (anonymize) a customer; order and invoice records and their contact data stay for the tax retention period.",
                    readOnly = false, destructive = "true", requiresConfirmation = true, order = 70)
        })
public final class ComplianceMcp {

    private ComplianceMcp() {}

    @McpTool(topic = "compliance", name = "services", description = "List the third-party services that receive shopper data in a store "
            + "(auto-detected from installed components, analytics, payment and carrier settings, plus manual entries).", readOnly = true, order = 10)
    public static Object listServices(McpCallContext ctx,
            @McpParam(name = "productStoreId", description = "Product store id; default: the store of the MCP session", required = false) String productStoreId)
            throws McpToolException {
        String storeId = storeId(ctx, productStoreId);
        List<ThirdPartyServiceRegistry.ServiceEntry> services = ThirdPartyServiceRegistry.getServices(ctx.getDelegator(), storeId);
        Map<String, Object> out = new LinkedHashMap<>();
        out.put("productStoreId", storeId);
        out.put("registryHash", ThirdPartyServiceRegistry.getRegistryHash(services));
        List<Map<String, String>> rows = new ArrayList<>();
        for (ThirdPartyServiceRegistry.ServiceEntry s : services) {
            rows.add(s.getFields());
        }
        out.put("services", rows);
        return out;
    }

    @McpTool(topic = "compliance", name = "documents", description = "List the legal texts of a store: published version, or 'template' "
            + "when the store shows the shipped template; flags a privacy or cookie policy that is older than the service list.", readOnly = true, order = 20)
    public static Object listDocuments(McpCallContext ctx,
            @McpParam(name = "productStoreId", description = "Product store id; default: the store of the MCP session", required = false) String productStoreId,
            @McpParam(name = "locale", description = "Locale, e.g. en or de", required = false) String locale)
            throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        String storeId = storeId(ctx, productStoreId);
        Locale loc = UtilValidate.isNotEmpty(locale) ? new Locale(locale) : (ctx.getLocale() != null ? ctx.getLocale() : Locale.ENGLISH);
        String currentHash = ThirdPartyServiceRegistry.getRegistryHash(delegator, storeId);
        List<Map<String, Object>> rows = new ArrayList<>();
        for (GenericValue type : LegalDocumentWorker.getDocTypes(delegator)) {
            Map<String, Object> row = new LinkedHashMap<>();
            row.put("docTypeId", type.getString("enumId"));
            row.put("slug", type.getString("enumCode"));
            GenericValue published = LegalDocumentWorker.getPublished(delegator, storeId, type.getString("enumId"), loc);
            if (published != null) {
                row.put("status", "published");
                row.put("versionNum", published.get("versionNum"));
                row.put("localeString", published.getString("localeString"));
                row.put("publishedDate", String.valueOf(published.getTimestamp("publishedDate")));
                boolean usesServices = "LEGDOC_PRIVACY".equals(type.getString("enumId")) || "LEGDOC_COOKIES".equals(type.getString("enumId"))
                        || "LEGDOC_CA_NOTICE".equals(type.getString("enumId"));
                row.put("outdated", usesServices && !currentHash.equals(published.getString("registryHash")));
            } else {
                row.put("status", LegalDocumentWorker.getTemplateText(type.getString("enumCode"), loc) != null ? "template" : "missing");
            }
            rows.add(row);
        }
        Map<String, Object> out = new LinkedHashMap<>();
        out.put("productStoreId", storeId);
        out.put("documents", rows);
        return out;
    }

    @McpTool(topic = "compliance", name = "checklist", description = "Run the compliance checklist of a store: legal texts, trader identity, "
            + "EPR registration, GPSR data, privacy requests, marketplace sellers. Each row: status OK/WARN/ERROR, detail, fix.", readOnly = true, order = 5)
    public static Object checklist(McpCallContext ctx,
            @McpParam(name = "productStoreId", description = "Product store id; default: the store of the MCP session", required = false) String productStoreId)
            throws McpToolException {
        String storeId = storeId(ctx, productStoreId);
        Map<String, Object> out = new LinkedHashMap<>();
        out.put("productStoreId", storeId);
        out.put("checks", com.ilscipio.scipio.compliance.ComplianceChecklist.run(ctx.getDelegator(), storeId, ctx.getLocale() != null ? ctx.getLocale() : Locale.ENGLISH));
        return out;
    }

    @McpTool(topic = "compliance", name = "requests", description = "List the open privacy requests of a store (access, delete, correct, opt-out) with due dates.", readOnly = true, order = 40)
    public static Object requests(McpCallContext ctx,
            @McpParam(name = "productStoreId", description = "Product store id; default: the store of the MCP session", required = false) String productStoreId)
            throws McpToolException {
        String storeId = storeId(ctx, productStoreId);
        List<Map<String, Object>> rows = new ArrayList<>();
        try {
            for (GenericValue r : org.ofbiz.entity.util.EntityQuery.use(ctx.getDelegator()).from("PrivacyRequest").where("productStoreId", storeId)
                    .orderBy("dueDate").queryList()) {
                if ("PRS_COMPLETED".equals(r.getString("statusId")) || "PRS_REJECTED".equals(r.getString("statusId"))) {
                    continue;
                }
                Map<String, Object> m = new LinkedHashMap<>();
                for (String f : new String[] {"privacyRequestId", "requestTypeId", "statusId", "partyId", "emailAddress", "jurisdiction"}) {
                    m.put(f, r.get(f));
                }
                m.put("receivedDate", String.valueOf(r.get("receivedDate")));
                m.put("dueDate", String.valueOf(r.get("dueDate")));
                rows.add(m);
            }
        } catch (org.ofbiz.entity.GenericEntityException e) {
            throw new McpToolException("Could not read the privacy requests: " + e.getMessage());
        }
        return rows;
    }

    @McpTool(topic = "compliance", name = "packaging_report", description = "Packaging placed on the market per destination country and material (kg), "
            + "from shipped boxes and items, for EPR declarations (PPWR).", readOnly = true, order = 50)
    public static Object packagingReport(McpCallContext ctx,
            @McpParam(name = "productStoreId", description = "Product store id; default: the store of the MCP session", required = false) String productStoreId,
            @McpParam(name = "days", description = "Period length in days back from today (default 90)", required = false) Integer days)
            throws McpToolException {
        String storeId = storeId(ctx, productStoreId);
        java.sql.Timestamp thru = org.ofbiz.base.util.UtilDateTime.nowTimestamp();
        java.sql.Timestamp from = org.ofbiz.base.util.UtilDateTime.adjustTimestamp(thru, java.util.Calendar.DAY_OF_YEAR, -(days != null ? days : 90));
        try {
            Map<String, Object> out = new LinkedHashMap<>();
            out.put("from", from.toString());
            out.put("thru", thru.toString());
            out.put("kgByCountryAndMaterial", com.ilscipio.scipio.compliance.PackagingReportWorker.placedOnMarket(ctx.getDelegator(), storeId, from, thru));
            return out;
        } catch (org.ofbiz.entity.GenericEntityException e) {
            throw new McpToolException("Could not build the packaging report: " + e.getMessage());
        }
    }

    private static String storeId(McpCallContext ctx, String productStoreId) throws McpToolException {
        String storeId = UtilValidate.isNotEmpty(productStoreId) ? productStoreId : ctx.getProductStoreId();
        if (UtilValidate.isEmpty(storeId)) {
            throw new McpToolException("productStoreId is required");
        }
        return storeId;
    }
}

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
package com.ilscipio.scipio.setup.mcp;

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
import com.ilscipio.scipio.mcp.def.McpResource;
import com.ilscipio.scipio.mcp.def.McpServer;
import com.ilscipio.scipio.mcp.def.McpServiceTool;
import com.ilscipio.scipio.mcp.def.McpTool;
import com.ilscipio.scipio.mcp.def.McpTopic;
import com.ilscipio.scipio.mcp.protocol.JsonRpc;
import com.ilscipio.scipio.mcp.registry.McpCallContext;
import com.ilscipio.scipio.mcp.registry.McpToolException;

/**
 * SCIPIO: 4.0.0: MCP server profile for the setup component: company, store and website configuration.
 */
@McpServer(name = "setup", title = "Scipio Setup", component = "setup",
        description = "Initial system setup: organization, stores, web sites, facilities and payments.",
        featuredServices = {"createProductStore", "updateProductStore", "createWebSite", "updateWebSite", "createPartyGroup",
                "createSetupTaxAuthority", "setupUpdatePartyAcctgPreference", "setupCreateOrganization", "setupLoadAccounting",
                "setupCreateFacility", "setupCreateStore", "setupCreateCatalog"},
        entities = {"ProductStore", "WebSite", "ProductStoreShipmentMeth", "ProductStorePaymentSetting", "PartyAcctgPreference",
                "ProductStoreCatalog", "ProductStoreFacility"},
        serviceTools = {
            @McpServiceTool(service = "updateProductStore", topic = "setup", name = "store_update", readOnly = false,
                    description = "Update product store settings, currency, locale and flags.",
                    requiresConfirmation = true, order = 30),
            @McpServiceTool(service = "setupCreateOrganization", topic = "setup", name = "organization", readOnly = false,
                    description = "Create the organization: address, telecom, currency, fiscal year, tax id.",
                    destructive = "false", order = 40),
            @McpServiceTool(service = "setupLoadAccounting", topic = "setup", name = "accounting", readOnly = false,
                    description = "Load the chart of accounts for an organization.",
                    destructive = "false", requiresConfirmation = true, order = 45),
            @McpServiceTool(service = "setupCreateFacility", topic = "setup", name = "facility", readOnly = false,
                    description = "Create a facility, such as a warehouse.",
                    destructive = "false", order = 41),
            @McpServiceTool(service = "setupCreateStore", topic = "setup", name = "store", readOnly = false,
                    description = "Create a product store, optionally with a web site.",
                    destructive = "false", order = 42),
            @McpServiceTool(service = "setupCreateCatalog", topic = "setup", name = "catalog", readOnly = false,
                    description = "Create a catalog for a product store.",
                    destructive = "false", order = 43),
            @McpServiceTool(service = "setupCreateUser", topic = "setup", name = "user", readOnly = false,
                    description = "Create an owner or admin user.",
                    destructive = "false", requiresConfirmation = true, order = 46),
            @McpServiceTool(service = "setupCreateTaxAuthority", topic = "setup", name = "tax_authority", readOnly = false,
                    description = "Create a tax authority and link it to an organization.",
                    destructive = "false", order = 47),
            @McpServiceTool(service = "createProductStore", topic = "setup", name = "store_create", readOnly = false,
                    description = "Create a product store at a low level.",
                    destructive = "false", order = 48)
        },
        topics = {
            @McpTopic(name = "setup", title = "Setup", order = 10, featured = true,
                    description = "System setup: organization, stores, sites, facilities, payments.")
        })
public final class SetupMcp {

    private SetupMcp() {}

    @McpTool(topic = "setup", name = "status", description = "Overview of setup: companies, accounting preferences, stores and sites.", readOnly = true, order = 10)
    public static Object status(McpCallContext ctx) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        try {
            Map<String, Object> out = new LinkedHashMap<>();
            List<Map<String, Object>> companies = new ArrayList<>();
            for (GenericValue pref : EntityQuery.use(delegator).from("PartyAcctgPreference").queryList()) {
                Map<String, Object> row = new LinkedHashMap<>();
                row.put("partyId", pref.getString("partyId"));
                GenericValue group = EntityQuery.use(delegator).from("PartyGroup").where("partyId", pref.getString("partyId")).cache().queryOne();
                row.put("groupName", group != null ? group.getString("groupName") : null);
                row.put("baseCurrencyUomId", pref.getString("baseCurrencyUomId"));
                companies.add(row);
            }
            out.put("companies", companies);
            List<Map<String, Object>> stores = new ArrayList<>();
            for (GenericValue store : EntityQuery.use(delegator).from("ProductStore").orderBy("productStoreId").queryList()) {
                Map<String, Object> row = new LinkedHashMap<>();
                row.put("productStoreId", store.getString("productStoreId"));
                row.put("storeName", store.getString("storeName"));
                row.put("payToPartyId", store.getString("payToPartyId"));
                row.put("defaultCurrencyUomId", store.getString("defaultCurrencyUomId"));
                List<String> sites = new ArrayList<>();
                for (GenericValue ws : EntityQuery.use(delegator).from("WebSite").where("productStoreId", store.getString("productStoreId")).queryList()) {
                    sites.add(ws.getString("webSiteId"));
                }
                row.put("webSiteIds", sites);
                row.put("shipmentMethods", EntityQuery.use(delegator).from("ProductStoreShipmentMeth")
                        .where("productStoreId", store.getString("productStoreId")).queryCount());
                row.put("paymentSettings", EntityQuery.use(delegator).from("ProductStorePaymentSetting")
                        .where("productStoreId", store.getString("productStoreId")).queryCount());
                stores.add(row);
            }
            out.put("productStores", stores);
            return out;
        } catch (GenericEntityException e) {
            throw new McpToolException("Setup status failed: " + e.getMessage());
        }
    }

    @McpTool(topic = "setup", name = "store_get", description = "Get one product store with sites, methods and catalogs.", readOnly = true, order = 20)
    public static Object getStore(McpCallContext ctx,
            @McpParam(name = "productStoreId", description = "Product store id, e.g. ScipioShop", required = true) String productStoreId) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        try {
            GenericValue store = EntityQuery.use(delegator).from("ProductStore").where("productStoreId", productStoreId).queryOne();
            if (store == null) throw new McpToolException("Product store not found: " + productStoreId);
            Map<String, Object> out = new LinkedHashMap<>();
            out.put("store", ResultConverter.toJson(store));
            out.put("webSites", ResultConverter.toJson(EntityQuery.use(delegator).from("WebSite").where("productStoreId", productStoreId).queryList()));
            out.put("shipmentMethods", ResultConverter.toJson(EntityQuery.use(delegator).from("ProductStoreShipmentMeth").where("productStoreId", productStoreId).queryList()));
            out.put("paymentSettings", ResultConverter.toJson(EntityQuery.use(delegator).from("ProductStorePaymentSetting").where("productStoreId", productStoreId).queryList()));
            out.put("catalogs", ResultConverter.toJson(EntityQuery.use(delegator).from("ProductStoreCatalog").where("productStoreId", productStoreId).filterByDate().queryList()));
            out.put("facilities", ResultConverter.toJson(EntityQuery.use(delegator).from("ProductStoreFacility").where("productStoreId", productStoreId).filterByDate().queryList()));
            return out;
        } catch (GenericEntityException e) {
            throw new McpToolException("Failed to load store " + productStoreId + ": " + e.getMessage());
        }
    }

    @McpTool(topic = "setup", name = "checklist", description = "Setup progress checklist with the next step to run.", readOnly = true, order = 10)
    public static Object checklist(McpCallContext ctx) throws McpToolException {
        try {
            return buildChecklist(ctx.getDelegator());
        } catch (GenericEntityException e) {
            throw new McpToolException("Setup checklist failed: " + e.getMessage());
        }
    }

    @McpResource(uri = "scipio://setup/status", name = "Setup status", description = "Setup progress checklist as JSON.",
            mimeType = "application/json")
    public static String statusResource(McpCallContext ctx) throws McpToolException {
        try {
            return JsonRpc.writePretty(buildChecklist(ctx.getDelegator()));
        } catch (GenericEntityException e) {
            throw new McpToolException("Setup checklist failed: " + e.getMessage());
        }
    }

    private static Map<String, Object> buildChecklist(Delegator delegator) throws GenericEntityException {
        List<Map<String, Object>> items = new ArrayList<>();
        String next = null;

        next = checklistItem(items, next, "organization", "PartyRole",
                EntityCondition.makeCondition("roleTypeId", "INTERNAL_ORGANIZATIO"), delegator,
                "call setup_organization");
        next = checklistItem(items, next, "accounting preferences", "PartyAcctgPreference", null, delegator,
                "call setup_organization with currencyUomId, or setup_accounting");
        next = checklistItem(items, next, "chart of accounts", "GlAccountOrganization", null, delegator,
                "call setup_accounting");
        next = checklistItem(items, next, "tax authority", "TaxAuthority", null, delegator,
                "call setup_tax_authority");
        next = checklistItem(items, next, "facility", "Facility", null, delegator,
                "call setup_facility");
        next = checklistItem(items, next, "store", "ProductStore", null, delegator,
                "call setup_store or store_create");
        next = checklistItem(items, next, "website", "WebSite", null, delegator,
                "call setup_store with webSiteName and hostname");
        next = checklistItem(items, next, "catalog", "ProdCatalog", null, delegator,
                "call setup_catalog");
        next = checklistItem(items, next, "category", "ProductCategory", null, delegator,
                "call setup_catalog with rootCategoryName, or category_create");
        next = checklistItem(items, next, "products", "Product", null, delegator,
                "create products in the catalog");
        next = checklistItem(items, next, "suppliers", "PartyRole",
                EntityCondition.makeCondition("roleTypeId", "SUPPLIER"), delegator,
                "assign the SUPPLIER role to a party");
        next = checklistItem(items, next, "bom", "ProductAssoc",
                EntityCondition.makeCondition("productAssocTypeId", "MANUF_COMPONENT"), delegator,
                "define a product bill of materials");
        next = checklistItem(items, next, "routings", "WorkEffort",
                EntityCondition.makeCondition("workEffortTypeId", "ROUTING"), delegator,
                "define a production routing");
        next = checklistItem(items, next, "work centers", "FixedAsset",
                EntityCondition.makeCondition("fixedAssetTypeId", EntityOperator.IN, List.of("PRODUCTION_EQUIPMENT", "GROUP_EQUIPMENT")),
                delegator, "create a production equipment or work center group");
        next = checklistItem(items, next, "users", "UserLogin",
                EntityCondition.makeCondition("userLoginId", EntityOperator.NOT_IN, List.of("system", "anonymous")),
                delegator, "call setup_user");
        next = checklistItem(items, next, "inventory", "InventoryItem", null, delegator,
                "receive inventory into the facility");
        next = checklistItem(items, next, "open orders", "OrderHeader",
                EntityCondition.makeCondition("statusId", EntityOperator.IN, List.of("ORDER_APPROVED", "ORDER_CREATED")),
                delegator, "create a sales order");
        next = checklistItem(items, next, "invoices", "Invoice", null, delegator,
                "create and post an invoice");
        next = checklistItem(items, next, "payment gateway", "ProductStorePaymentSetting", null, delegator,
                "call payment_provider_connect or store_update");
        next = checklistItem(items, next, "shipment methods", "ProductStoreShipmentMeth", null, delegator,
                "configure a shipment method for the store");

        Map<String, Object> out = new LinkedHashMap<>();
        out.put("items", items);
        out.put("next", next);
        return out;
    }

    /** Appends one checklist row and returns the running "next" hint (the first not-done item's hint). */
    private static String checklistItem(List<Map<String, Object>> items, String next, String item, String entityName,
            EntityCondition condition, Delegator delegator, String hint) throws GenericEntityException {
        EntityQuery query = EntityQuery.use(delegator).from(entityName);
        if (condition != null) {
            query = query.where(condition);
        }
        long count = query.queryCount();
        boolean done = count > 0;
        Map<String, Object> row = new LinkedHashMap<>();
        row.put("item", item);
        row.put("done", done);
        row.put("count", count);
        row.put("hint", hint);
        items.add(row);
        return (next == null && !done) ? hint : next;
    }

    private static final Map<String, String[]> PAYMENT_PROVIDERS = Map.of(
            "PAYPAL", new String[] {"updatePaymentGatewayConfigPayPal", "PAYPAL_CONFIG"},
            "AUTHORIZE_NET", new String[] {"updatePaymentGatewayConfigAuthorizeNet", "AUTHORIZE_NET_CONFIG"},
            "PAYFLOWPRO", new String[] {"updatePaymentGatewayConfigPayflowPro", "PAYFLOWPRO_CONFIG"},
            "SECUREPAY", new String[] {"updatePaymentGatewayConfigSecurePay", "SECUREPAY_CONFIG"},
            "SAGEPAY", new String[] {"updatePaymentGatewayConfigSagePay", "SAGEPAY_CONFIG"},
            "WORLDPAY", new String[] {"updatePaymentGatewayConfigWorldPay", "WORLDPAY_CONFIG"},
            "IDEAL", new String[] {"updatePaymentGatewayConfigiDEAL", "IDEAL_CONFIG"});

    @McpTool(topic = "setup", name = "payment_provider_connect", description = "Connect a payment gateway to a product store.", readOnly = false, destructive = "true", permission = "PAYPROC_ADMIN", requiresConfirmation = true, order = 75)
    public static Object connectPaymentProvider(McpCallContext ctx,
            @McpParam(name = "providerId", description = "Gateway provider", required = true,
                    enumValues = {"PAYPAL", "AUTHORIZE_NET", "PAYFLOWPRO", "SECUREPAY", "SAGEPAY", "WORLDPAY", "IDEAL"}) String providerId,
            @McpParam(name = "productStoreId", required = true) String productStoreId,
            @McpParam(name = "paymentMethodTypeId", description = "Defaults to CREDIT_CARD (EXT_PAYPAL for PAYPAL)", required = false) String paymentMethodTypeId,
            @McpParam(name = "config", description = "Gateway config fields (credentials, mode, urls, ...)", required = true, type = "object") Map<String, Object> config)
            throws McpToolException {
        String[] mapping = PAYMENT_PROVIDERS.get(providerId);
        if (mapping == null) {
            throw new McpToolException("Provider not wired yet: " + providerId);
        }
        String configService = mapping[0];
        String paymentGatewayConfigId = mapping[1];

        Map<String, Object> configCtx = new LinkedHashMap<>();
        if (config != null) {
            configCtx.putAll(config);
        }
        configCtx.put("paymentGatewayConfigId", paymentGatewayConfigId);
        ctx.runService(configService, configCtx);

        String resolvedMethodTypeId = paymentMethodTypeId;
        String paymentServiceTypeEnumId = "PRDS_PAY_AUTH";
        if (resolvedMethodTypeId == null || resolvedMethodTypeId.isEmpty()) {
            if ("PAYPAL".equals(providerId)) {
                resolvedMethodTypeId = "EXT_PAYPAL";
                paymentServiceTypeEnumId = "PRDS_PAY_EXTERNAL";
            } else {
                resolvedMethodTypeId = "CREDIT_CARD";
            }
        }

        Map<String, Object> settingCtx = new LinkedHashMap<>();
        settingCtx.put("productStoreId", productStoreId);
        settingCtx.put("paymentMethodTypeId", resolvedMethodTypeId);
        settingCtx.put("paymentServiceTypeEnumId", paymentServiceTypeEnumId);
        settingCtx.put("paymentGatewayConfigId", paymentGatewayConfigId);
        ctx.runService("createProductStorePaymentSetting", settingCtx);

        Map<String, Object> out = new LinkedHashMap<>();
        out.put("providerId", providerId);
        out.put("productStoreId", productStoreId);
        out.put("paymentMethodTypeId", resolvedMethodTypeId);
        out.put("paymentServiceTypeEnumId", paymentServiceTypeEnumId);
        out.put("paymentGatewayConfigId", paymentGatewayConfigId);
        return out;
    }
}

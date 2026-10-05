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
package com.ilscipio.scipio.product.mcp;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.HashMap;
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
import com.ilscipio.scipio.mcp.registry.McpResult;
import com.ilscipio.scipio.mcp.registry.McpToolException;
import com.ilscipio.scipio.mcp.tool.DocumentTools;

/**
 * SCIPIO: 4.0.0: MCP server profile for the facility webapp: warehouse facilities, inventory and shipments.
 */
@McpServer(name = "facility", title = "Scipio Facility & Inventory", component = "product", webapps = {"facility"},
        description = "Facility and inventory: stock, receiving, transfers, reorder points and shipments.",
        featuredServices = {"createInventoryItem", "getInventoryAvailableByFacility", "getProductInventoryAvailable",
                "reserveProductInventory", "createShipment", "updateShipment", "quickDropShipOrder", "createPhysicalInventory",
                "receiveInventoryProduct", "quickShipEntireOrder"},
        entities = {"Facility", "InventoryItem", "InventoryItemDetail", "Shipment", "ShipmentItem", "ShipmentStatus",
                "ShipmentRouteSegment", "ShipmentPackage", "FacilityLocation", "ProductFacility"},
        serviceTools = {
            @McpServiceTool(service = "reserveProductInventory",
                    topic = "inventory",
                    name = "reserve",
                    description = "Reserve product inventory for an order or requirement.",
                    readOnly = false,
                    destructive = "false",
                    order = 66),
            @McpServiceTool(service = "createInventoryItem",
                    topic = "inventory",
                    name = "item_create",
                    description = "Create an inventory item for a product.",
                    readOnly = false,
                    destructive = "false",
                    order = 44),
            @McpServiceTool(service = "quickShipEntireOrder",
                    topic = "shipment",
                    name = "ship_order",
                    description = "Ship an entire order in one step.",
                    readOnly = false,
                    destructive = "true",
                    requiresConfirmation = true,
                    order = 90),
            @McpServiceTool(service = "quickDropShipOrder",
                    topic = "shipment",
                    name = "drop_ship",
                    description = "Drop-ship an order directly from a supplier.",
                    readOnly = false,
                    destructive = "true",
                    requiresConfirmation = true,
                    order = 91),
            @McpServiceTool(service = "receiveInventoryProduct", topic = "inventory", name = "receive",
                    description = "Receive product quantity into a facility.", readOnly = false, requiresConfirmation = true, order = 30),
            @McpServiceTool(service = "createPhysicalInventory", topic = "inventory", name = "adjust_start",
                    description = "Start a physical inventory count for later variance.",
                    readOnly = false, order = 40),
            @McpServiceTool(service = "createFacility", topic = "inventory", name = "facility_create",
                    description = "Create a facility, such as a warehouse or store.", readOnly = false, destructive = "false", order = 45),
            @McpServiceTool(service = "createFacilityLocation", topic = "inventory", name = "facility_location_create",
                    description = "Create a storage location inside a facility.",
                    readOnly = false, destructive = "false", order = 48),
            @McpServiceTool(service = "createFacilityPostalAddress", topic = "inventory", name = "facility_address_set",
                    description = "Add a postal address to a facility.",
                    readOnly = false, destructive = "false", order = 50),
            @McpServiceTool(service = "createProductFacility", topic = "inventory", name = "facility_product_set",
                    description = "Link a product to a facility with stocking rules.",
                    readOnly = false, destructive = "false", order = 52),
            @McpServiceTool(service = "createShipment", topic = "shipment", name = "create",
                    description = "Create a shipment for an order.",
                    readOnly = false, destructive = "false", order = 55),
            @McpServiceTool(service = "createShipmentPackage", topic = "shipment", name = "package_add",
                    description = "Add a package to a shipment.",
                    readOnly = false, destructive = "false", order = 58),
            @McpServiceTool(service = "updateShipmentRouteSegment", topic = "shipment", name = "route_update",
                    description = "Update a shipment route segment's carrier or tracking.",
                    readOnly = false, destructive = "false", order = 60),
            @McpServiceTool(service = "createInventoryTransfer", topic = "inventory", name = "transfer_create",
                    description = "Start an inventory transfer between locations.", readOnly = false, destructive = "false", order = 62),
            @McpServiceTool(service = "updateInventoryItem", topic = "inventory", name = "item_update",
                    description = "Update an inventory item's status, facility or location.",
                    readOnly = false, destructive = "false", order = 64),
            @McpServiceTool(service = "updateShipment", topic = "shipment", name = "update",
                    description = "Update a shipment's status or destination.",
                    readOnly = false, destructive = "true", requiresConfirmation = true, order = 70),
            @McpServiceTool(service = "updateShipment", topic = "shipment", name = "pack",
                    description = "Mark a shipment as packed (status SHIPMENT_PACKED).",
                    readOnly = false, destructive = "false", fixed = {"statusId=SHIPMENT_PACKED"}, order = 71),
            @McpServiceTool(service = "updateInventoryTransfer", topic = "inventory", name = "transfer_update",
                    description = "Update an inventory transfer's status or receive date.",
                    readOnly = false, destructive = "true", requiresConfirmation = true, order = 75)
        },
        topics = {
            @McpTopic(name = "inventory", title = "Inventory and facilities", order = 10, featured = true,
                    description = "Inventory, stock counts, transfers, reorder points and facility setup."),
            @McpTopic(name = "shipment", title = "Shipments", order = 20, featured = true,
                    description = "Shipments: find, plan, create, update, pack, label, ship.")
        })
public final class FacilityMcp {

    private FacilityMcp() {}

    @McpTool(topic = "inventory", name = "get", description = "Get available-to-promise and on-hand quantities for a product.", readOnly = true, order = 10)
    public static Object getInventory(McpCallContext ctx,
            @McpParam(name = "productId", required = true) String productId,
            @McpParam(name = "facilityId", description = "One facility; omit for every facility", required = false) String facilityId) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        try {
            List<GenericValue> facilities;
            if (facilityId != null) {
                GenericValue f = EntityQuery.use(delegator).from("Facility").where("facilityId", facilityId).cache().queryOne();
                if (f == null) throw new McpToolException("Facility not found: " + facilityId);
                facilities = java.util.Collections.singletonList(f);
            } else {
                facilities = EntityQuery.use(delegator).from("Facility").orderBy("facilityId").queryList();
            }
            List<Map<String, Object>> rows = new ArrayList<>();
            for (GenericValue f : facilities) {
                Map<String, Object> params = new LinkedHashMap<>();
                params.put("productId", productId);
                params.put("facilityId", f.getString("facilityId"));
                Map<String, Object> res = ctx.runService("getInventoryAvailableByFacility", params);
                Map<String, Object> row = new LinkedHashMap<>();
                row.put("facilityId", f.getString("facilityId"));
                row.put("facilityName", f.getString("facilityName"));
                row.put("availableToPromiseTotal", ResultConverter.toJson(res.get("availableToPromiseTotal")));
                row.put("quantityOnHandTotal", ResultConverter.toJson(res.get("quantityOnHandTotal")));
                rows.add(row);
            }
            Map<String, Object> out = new LinkedHashMap<>();
            out.put("productId", productId);
            out.put("facilities", rows);
            return out;
        } catch (GenericEntityException e) {
            throw new McpToolException("Inventory lookup failed: " + e.getMessage());
        }
    }

    @McpTool(topic = "shipment", name = "find", description = "Find shipments by id, status, type or destination party.", readOnly = true, order = 20)
    public static Object findShipments(McpCallContext ctx,
            @McpParam(name = "shipmentId", required = false) String shipmentId,
            @McpParam(name = "statusId", description = "e.g. SHIPMENT_INPUT, SHIPMENT_PACKED, SHIPMENT_SHIPPED", required = false) String statusId,
            @McpParam(name = "shipmentTypeId", description = "e.g. SALES_SHIPMENT, PURCHASE_SHIPMENT", required = false) String shipmentTypeId,
            @McpParam(name = "primaryOrderId", required = false) String primaryOrderId,
            @McpParam(name = "partyIdTo", required = false) String partyIdTo,
            @McpParam(name = "limit", required = false) Integer limit) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        try {
            List<EntityCondition> conds = new ArrayList<>();
            if (shipmentId != null) conds.add(EntityCondition.makeCondition("shipmentId", shipmentId));
            if (statusId != null) conds.add(EntityCondition.makeCondition("statusId", statusId));
            if (shipmentTypeId != null) conds.add(EntityCondition.makeCondition("shipmentTypeId", shipmentTypeId));
            if (primaryOrderId != null) conds.add(EntityCondition.makeCondition("primaryOrderId", primaryOrderId));
            if (partyIdTo != null) conds.add(EntityCondition.makeCondition("partyIdTo", partyIdTo));
            return ResultConverter.toJson(EntityQuery.use(delegator).from("Shipment").where(conds)
                    .orderBy("-createdStamp").maxRows(ctx.limit(limit)).queryList());
        } catch (GenericEntityException e) {
            throw new McpToolException("Shipment search failed: " + e.getMessage());
        }
    }

    @McpTool(topic = "shipment", name = "get", description = "Get one shipment with items, status history and routes.", readOnly = true, order = 25)
    public static Object getShipment(McpCallContext ctx,
            @McpParam(name = "shipmentId", required = true) String shipmentId) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        try {
            GenericValue shipment = EntityQuery.use(delegator).from("Shipment").where("shipmentId", shipmentId).queryOne();
            if (shipment == null) throw new McpToolException("Shipment not found: " + shipmentId);
            Map<String, Object> out = new LinkedHashMap<>();
            out.put("shipment", ResultConverter.toJson(shipment));
            out.put("items", ResultConverter.toJson(EntityQuery.use(delegator).from("ShipmentItem").where("shipmentId", shipmentId).queryList()));
            out.put("packages", ResultConverter.toJson(EntityQuery.use(delegator).from("ShipmentPackage").where("shipmentId", shipmentId).queryList()));
            out.put("statusHistory", ResultConverter.toJson(EntityQuery.use(delegator).from("ShipmentStatus").where("shipmentId", shipmentId).orderBy("statusDate").queryList()));
            out.put("routeSegments", ResultConverter.toJson(EntityQuery.use(delegator).from("ShipmentRouteSegment").where("shipmentId", shipmentId).queryList()));
            return out;
        } catch (GenericEntityException e) {
            throw new McpToolException("Failed to load shipment " + shipmentId + ": " + e.getMessage());
        }
    }

    @McpResource(uri = "scipio://shipment/{shipmentId}", name = "Shipment",
            description = "One shipment as JSON: header, items, packages, route segments, status history.",
            mimeType = "application/json")
    public static String shipmentResource(McpCallContext ctx, Map<String, String> uriParams) throws McpToolException {
        return JsonRpc.writePretty(getShipment(ctx, uriParams.get("shipmentId")));
    }

    @McpTool(topic = "shipment", name = "plan", description = "Estimate shipping cost per carrier for an order or address.", readOnly = true, order = 22)
    public static Object planShipment(McpCallContext ctx,
            @McpParam(name = "orderId", description = "Resolve productStoreId, shippingContactMechId and items from this order", required = false) String orderId,
            @McpParam(name = "productStoreId", required = false) String productStoreId,
            @McpParam(name = "shippingContactMechId", description = "Ship-to PostalAddress contactMechId", required = false) String shippingContactMechId,
            @McpParam(name = "items", description = "Array of {productId, quantity}", required = false, type = "array") List<Object> items,
            @McpParam(name = "weight", description = "Total shippable weight; default 0", required = false) BigDecimal weight) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        try {
            BigDecimal shippableQuantity = BigDecimal.ZERO;
            List<Map<String, Object>> shippableItemInfo = new ArrayList<>();
            if (orderId != null) {
                GenericValue order = EntityQuery.use(delegator).from("OrderHeader").where("orderId", orderId).queryOne();
                if (order == null) throw new McpToolException("Order not found: " + orderId);
                productStoreId = order.getString("productStoreId");
                GenericValue ocm = EntityQuery.use(delegator).from("OrderContactMech")
                        .where("orderId", orderId, "contactMechPurposeTypeId", "SHIPPING_LOCATION").queryFirst();
                if (ocm != null) shippingContactMechId = ocm.getString("contactMechId");
                List<GenericValue> orderItems = EntityQuery.use(delegator).from("OrderItem").where("orderId", orderId).queryList();
                for (GenericValue oi : orderItems) {
                    BigDecimal qty = oi.getBigDecimal("quantity");
                    if (qty == null) continue;
                    shippableQuantity = shippableQuantity.add(qty);
                    Map<String, Object> info = new LinkedHashMap<>();
                    info.put("productId", oi.getString("productId"));
                    info.put("quantity", qty);
                    shippableItemInfo.add(info);
                }
            } else if (items != null) {
                for (Object o : items) {
                    Map<String, Object> item = asMap(o);
                    BigDecimal qty = toBigDecimal(item.get("quantity"));
                    if (qty == null) qty = BigDecimal.ONE;
                    shippableQuantity = shippableQuantity.add(qty);
                    Map<String, Object> info = new LinkedHashMap<>();
                    info.put("productId", item.get("productId"));
                    info.put("quantity", qty);
                    shippableItemInfo.add(info);
                }
            }
            if (productStoreId == null) throw new McpToolException("orderId or productStoreId is required");
            if (shippingContactMechId == null) throw new McpToolException("shippingContactMechId is required (directly, or resolved from orderId)");
            BigDecimal shippableWeight = weight != null ? weight : BigDecimal.ZERO;

            // SCIPIO: 4.0.0: ProductStoreShipmentMeth carries no date range, so no date filter
            List<GenericValue> shipMeths = EntityQuery.use(delegator).from("ProductStoreShipmentMeth")
                    .where("productStoreId", productStoreId, "roleTypeId", "CARRIER").queryList();

            List<Map<String, Object>> options = new ArrayList<>();
            List<String> errors = new ArrayList<>();
            for (GenericValue meth : shipMeths) {
                String carrierPartyId = meth.getString("partyId");
                String shipmentMethodTypeId = meth.getString("shipmentMethodTypeId");
                Map<String, Object> params = new LinkedHashMap<>();
                params.put("productStoreId", productStoreId);
                params.put("carrierPartyId", carrierPartyId);
                params.put("carrierRoleTypeId", "CARRIER");
                params.put("shipmentMethodTypeId", shipmentMethodTypeId);
                params.put("shippingContactMechId", shippingContactMechId);
                params.put("shippableItemInfo", shippableItemInfo);
                params.put("shippableTotal", BigDecimal.ZERO);
                params.put("shippableQuantity", shippableQuantity);
                params.put("shippableWeight", shippableWeight);
                params.put("initialEstimateAmt", BigDecimal.ZERO);
                try {
                    Map<String, Object> res = ctx.runService("calcShipmentCostEstimate", params);
                    Object amt = res.get("shippingEstimateAmount");
                    if (!(amt instanceof BigDecimal)) {
                        errors.add(shipmentMethodTypeId + "/" + carrierPartyId + ": no estimate found");
                        continue;
                    }
                    GenericValue methodType = EntityQuery.use(delegator).from("ShipmentMethodType")
                            .where("shipmentMethodTypeId", shipmentMethodTypeId).cache().queryOne();
                    Map<String, Object> option = new LinkedHashMap<>();
                    option.put("carrierPartyId", carrierPartyId);
                    option.put("shipmentMethodTypeId", shipmentMethodTypeId);
                    option.put("description", methodType != null ? methodType.getString("description") : shipmentMethodTypeId);
                    option.put("amount", amt);
                    option.put("currencyUomId", ctx.getCurrencyUomId());
                    options.add(option);
                } catch (McpToolException e) {
                    errors.add(shipmentMethodTypeId + "/" + carrierPartyId + ": " + e.getMessage());
                }
            }
            options.sort((a, b) -> ((BigDecimal) a.get("amount")).compareTo((BigDecimal) b.get("amount")));

            Map<String, Object> out = new LinkedHashMap<>();
            out.put("orderId", orderId);
            out.put("productStoreId", productStoreId);
            out.put("options", options);
            if (!options.isEmpty()) out.put("cheapest", options.get(0));
            out.put("errors", errors);
            return ResultConverter.toJson(out);
        } catch (GenericEntityException e) {
            throw new McpToolException("Shipment plan failed: " + e.getMessage());
        }
    }

    @McpTool(topic = "shipment", name = "label_create", description = "Create the shipping label for a shipment route segment.",
            readOnly = false, destructive = "true", requiresConfirmation = true, order = 85)
    public static Object createLabel(McpCallContext ctx,
            @McpParam(name = "shipmentId", required = true) String shipmentId,
            @McpParam(name = "shipmentRouteSegmentId", required = false) String shipmentRouteSegmentId) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        String segId = shipmentRouteSegmentId != null ? shipmentRouteSegmentId : "00001";
        try {
            GenericValue segment = EntityQuery.use(delegator).from("ShipmentRouteSegment")
                    .where("shipmentId", shipmentId, "shipmentRouteSegmentId", segId).queryOne();
            if (segment == null) throw new McpToolException("Shipment route segment not found: " + shipmentId + "/" + segId);
            String carrierPartyId = segment.getString("carrierPartyId");
            if ("FEDEX".equals(carrierPartyId)) {
                Map<String, Object> params = new LinkedHashMap<>();
                params.put("shipmentId", shipmentId);
                params.put("shipmentRouteSegmentId", segId);
                Map<String, Object> res = ctx.runService("fedexShipRequest", params);
                GenericValue reloaded = EntityQuery.use(delegator).from("ShipmentRouteSegment")
                        .where("shipmentId", shipmentId, "shipmentRouteSegmentId", segId).queryOne();
                Map<String, Object> out = new LinkedHashMap<>();
                out.put("result", res);
                out.put("trackingIdNumber", reloaded != null ? reloaded.getString("trackingIdNumber") : null);
                return ResultConverter.toJson(out);
            }
            byte[] pdf = DocumentTools.render(ctx, DocumentTools.docType("shipment_label"), shipmentId);
            Map<String, Object> info = new LinkedHashMap<>();
            info.put("shipmentId", shipmentId);
            info.put("shipmentRouteSegmentId", segId);
            info.put("carrierPartyId", carrierPartyId);
            info.put("note", "Carrier " + carrierPartyId + " has no label integration in core; a generic label was rendered; "
                    + "set the tracking number with shipment_route_update.");
            return McpResult.blob(ResultConverter.toJson(info), pdf, "application/pdf", "shipment-label-" + shipmentId + ".pdf");
        } catch (GenericEntityException e) {
            throw new McpToolException("Label creation failed: " + e.getMessage());
        }
    }

    /** Carrier that has a void service in core: UPS (upsVoidShipment). Others: label_void_not_supported. */
    static final String VOID_CARRIER = "UPS";

    @McpTool(topic = "shipment", name = "label_void", description = "Void the shipping label of a shipment route segment (UPS only; other carriers: label_void_not_supported).",
            readOnly = false, destructive = "true", requiresConfirmation = true, order = 86)
    public static Object voidLabel(McpCallContext ctx,
            @McpParam(name = "shipmentId", required = true) String shipmentId,
            @McpParam(name = "shipmentRouteSegmentId", description = "Default: 00001", required = false) String shipmentRouteSegmentId) throws McpToolException {
        ctx.requirePermission("FACILITY_UPDATE");
        String segId = shipmentRouteSegmentId != null ? shipmentRouteSegmentId : "00001";
        try {
            GenericValue segment = EntityQuery.use(ctx.getDelegator()).from("ShipmentRouteSegment")
                    .where("shipmentId", shipmentId, "shipmentRouteSegmentId", segId).queryOne();
            if (segment == null) throw new McpToolException("Shipment route segment not found: " + shipmentId + "/" + segId);
            String carrier = segment.getString("carrierPartyId");
            if (!VOID_CARRIER.equals(carrier)) {
                throw new McpToolException("label_void_not_supported: carrier " + carrier
                        + " has no void service in core (carrier labels come with W1-11).");
            }
            Map<String, Object> params = new LinkedHashMap<>();
            params.put("shipmentId", shipmentId);
            params.put("shipmentRouteSegmentId", segId);
            return ResultConverter.toJsonMap(ctx.runService("upsVoidShipment", params));
        } catch (GenericEntityException e) {
            throw new McpToolException("Label void failed: " + e.getMessage());
        }
    }

    @McpTool(topic = "inventory", name = "reorder_point", description = "Analyze reorder points: stock, velocity, lead time, suggested quantity.", readOnly = true, order = 15)
    public static Object getReorderPoints(McpCallContext ctx,
            @McpParam(name = "facilityId", required = true) String facilityId,
            @McpParam(name = "productId", description = "One product; omit for every stocked product", required = false) String productId,
            @McpParam(name = "days", description = "Sales-velocity lookback window in days; default 30", required = false) Integer days,
            @McpParam(name = "limit", required = false) Integer limit) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        int lookbackDays = (days != null && days > 0) ? days : 30;
        try {
            List<EntityCondition> pfConds = new ArrayList<>();
            pfConds.add(EntityCondition.makeCondition("facilityId", facilityId));
            if (productId != null) pfConds.add(EntityCondition.makeCondition("productId", productId));
            List<GenericValue> productFacilities = EntityQuery.use(delegator).from("ProductFacility").where(pfConds).queryList();

            Timestamp since = new Timestamp(System.currentTimeMillis() - lookbackDays * 86400000L);
            List<EntityCondition> ohConds = new ArrayList<>();
            ohConds.add(EntityCondition.makeCondition("orderTypeId", "SALES_ORDER"));
            ohConds.add(EntityCondition.makeCondition("statusId", EntityOperator.IN,
                    java.util.Arrays.asList("ORDER_APPROVED", "ORDER_COMPLETED")));
            ohConds.add(EntityCondition.makeCondition("orderDate", EntityOperator.GREATER_THAN_EQUAL_TO, since));
            List<GenericValue> orders = EntityQuery.use(delegator).from("OrderHeader").where(ohConds).queryList();
            List<String> orderIds = new ArrayList<>();
            for (GenericValue oh : orders) orderIds.add(oh.getString("orderId"));

            Map<String, BigDecimal> salesByProduct = new HashMap<>();
            if (!orderIds.isEmpty()) {
                List<EntityCondition> oiConds = new ArrayList<>();
                oiConds.add(EntityCondition.makeCondition("orderId", EntityOperator.IN, orderIds));
                if (productId != null) oiConds.add(EntityCondition.makeCondition("productId", productId));
                List<GenericValue> orderItems = EntityQuery.use(delegator).from("OrderItem").where(oiConds).queryList();
                for (GenericValue oi : orderItems) {
                    String pid = oi.getString("productId");
                    BigDecimal qty = oi.getBigDecimal("quantity");
                    if (pid == null || qty == null) continue;
                    salesByProduct.merge(pid, qty, BigDecimal::add);
                }
            }

            List<Map<String, Object>> rows = new ArrayList<>();
            for (GenericValue pf : productFacilities) {
                String pid = pf.getString("productId");
                BigDecimal minimumStock = pf.getBigDecimal("minimumStock");
                BigDecimal reorderQuantity = pf.getBigDecimal("reorderQuantity");
                Long daysToShip = pf.getLong("daysToShip");

                Map<String, Object> invParams = new LinkedHashMap<>();
                invParams.put("productId", pid);
                invParams.put("facilityId", facilityId);
                Map<String, Object> invRes = ctx.runService("getInventoryAvailableByFacility", invParams);
                BigDecimal atp = (BigDecimal) invRes.get("availableToPromiseTotal");
                BigDecimal qoh = (BigDecimal) invRes.get("quantityOnHandTotal");
                if (atp == null) atp = BigDecimal.ZERO;
                if (qoh == null) qoh = BigDecimal.ZERO;

                BigDecimal sold = salesByProduct.getOrDefault(pid, BigDecimal.ZERO);
                BigDecimal velocity = sold.divide(BigDecimal.valueOf(lookbackDays), 6, RoundingMode.HALF_UP);

                List<GenericValue> supplierProducts = EntityQuery.use(delegator).from("SupplierProduct")
                        .where("productId", pid).filterByDate("availableFromDate", "availableThruDate").queryList();
                Long leadDays = null;
                for (GenericValue sp : supplierProducts) {
                    BigDecimal lt = sp.getBigDecimal("standardLeadTimeDays");
                    if (lt != null) {
                        long v = lt.longValue();
                        if (leadDays == null || v < leadDays) leadDays = v;
                    }
                }
                long effectiveLeadDays = leadDays != null ? leadDays : 0L;

                BigDecimal daysOfCover = null;
                if (velocity.compareTo(BigDecimal.ZERO) > 0) {
                    daysOfCover = atp.divide(velocity, 2, RoundingMode.HALF_UP);
                }

                boolean lowStock = minimumStock != null && atp.compareTo(minimumStock) <= 0;
                boolean lowCover = daysOfCover != null && daysOfCover.compareTo(BigDecimal.valueOf(effectiveLeadDays + 7)) < 0;
                BigDecimal suggestedQuantity = BigDecimal.ZERO;
                if (lowStock || lowCover) {
                    BigDecimal needed = velocity.multiply(BigDecimal.valueOf(effectiveLeadDays + 14))
                            .setScale(0, RoundingMode.CEILING).subtract(atp);
                    BigDecimal reorderQty = reorderQuantity != null ? reorderQuantity : BigDecimal.ZERO;
                    suggestedQuantity = needed.max(reorderQty);
                }

                Map<String, Object> row = new LinkedHashMap<>();
                row.put("productId", pid);
                row.put("facilityId", facilityId);
                row.put("minimumStock", minimumStock);
                row.put("reorderQuantity", reorderQuantity);
                row.put("daysToShip", daysToShip);
                row.put("quantityOnHand", qoh);
                row.put("availableToPromise", atp);
                row.put("salesVelocityPerDay", velocity);
                row.put("supplierLeadTimeDays", leadDays);
                row.put("daysOfCover", daysOfCover);
                row.put("suggestedQuantity", suggestedQuantity);
                rows.add(row);
            }

            rows.sort((a, b) -> {
                BigDecimal da = (BigDecimal) a.get("daysOfCover");
                BigDecimal db = (BigDecimal) b.get("daysOfCover");
                if (da == null && db == null) return 0;
                if (da == null) return 1;
                if (db == null) return -1;
                return da.compareTo(db);
            });
            int lim = ctx.limit(limit);
            if (rows.size() > lim) rows = rows.subList(0, lim);

            List<Map<String, Object>> reorder = new ArrayList<>();
            for (Map<String, Object> row : rows) {
                if (((BigDecimal) row.get("suggestedQuantity")).compareTo(BigDecimal.ZERO) > 0) reorder.add(row);
            }

            Map<String, Object> out = new LinkedHashMap<>();
            out.put("facilityId", facilityId);
            out.put("days", lookbackDays);
            out.put("rows", rows);
            out.put("reorder", reorder);
            return ResultConverter.toJson(out);
        } catch (GenericEntityException e) {
            throw new McpToolException("Reorder point lookup failed: " + e.getMessage());
        }
    }

    @McpTool(topic = "inventory", name = "count_apply", description = "Preview or apply a physical inventory count against stock.",
            readOnly = false, destructive = "true", requiresConfirmation = true, order = 80)
    public static Object applyInventoryCount(McpCallContext ctx,
            @McpParam(name = "facilityId", required = true) String facilityId,
            @McpParam(name = "rows", description = "Array of {productId, quantityOnHand, locationSeqId (optional), comments (optional)}", required = true, type = "array") List<Object> rows,
            @McpParam(name = "apply", description = "Default false: preview only, do not write", required = false) Boolean apply) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        boolean doApply = Boolean.TRUE.equals(apply);
        List<Map<String, Object>> results = new ArrayList<>();
        try {
            for (Object o : rows) {
                Map<String, Object> row = asMap(o);
                String productId = String.valueOf(row.get("productId"));
                BigDecimal countedQty = toBigDecimal(row.get("quantityOnHand"));
                if (countedQty == null) throw new McpToolException("Row is missing quantityOnHand for productId " + productId);
                String locationSeqId = row.get("locationSeqId") != null ? String.valueOf(row.get("locationSeqId")) : null;
                String comments = row.get("comments") != null ? String.valueOf(row.get("comments")) : null;

                Map<String, Object> invParams = new LinkedHashMap<>();
                invParams.put("productId", productId);
                invParams.put("facilityId", facilityId);
                Map<String, Object> invRes = ctx.runService("getInventoryAvailableByFacility", invParams);
                BigDecimal currentQoh = (BigDecimal) invRes.get("quantityOnHandTotal");
                if (currentQoh == null) currentQoh = BigDecimal.ZERO;
                BigDecimal variance = countedQty.subtract(currentQoh);

                Map<String, Object> result = new LinkedHashMap<>();
                result.put("productId", productId);
                result.put("locationSeqId", locationSeqId);
                result.put("currentQuantityOnHand", currentQoh);
                result.put("countedQuantityOnHand", countedQty);
                result.put("variance", variance);

                if (doApply && variance.compareTo(BigDecimal.ZERO) != 0) {
                    List<EntityCondition> iiConds = new ArrayList<>();
                    iiConds.add(EntityCondition.makeCondition("productId", productId));
                    iiConds.add(EntityCondition.makeCondition("facilityId", facilityId));
                    iiConds.add(EntityCondition.makeCondition("inventoryItemTypeId", "NON_SERIAL_INV_ITEM"));
                    if (locationSeqId != null) iiConds.add(EntityCondition.makeCondition("locationSeqId", locationSeqId));
                    List<GenericValue> items = EntityQuery.use(delegator).from("InventoryItem").where(iiConds)
                            .orderBy("-quantityOnHandTotal").queryList();
                    String inventoryItemId;
                    if (!items.isEmpty()) {
                        inventoryItemId = items.get(0).getString("inventoryItemId");
                    } else {
                        Map<String, Object> createParams = new LinkedHashMap<>();
                        createParams.put("productId", productId);
                        createParams.put("facilityId", facilityId);
                        createParams.put("inventoryItemTypeId", "NON_SERIAL_INV_ITEM");
                        if (locationSeqId != null) createParams.put("locationSeqId", locationSeqId);
                        Map<String, Object> createRes = ctx.runService("createInventoryItem", createParams);
                        inventoryItemId = (String) createRes.get("inventoryItemId");
                    }
                    Map<String, Object> varParams = new LinkedHashMap<>();
                    varParams.put("inventoryItemId", inventoryItemId);
                    varParams.put("physicalInventoryDate", new Timestamp(System.currentTimeMillis()));
                    varParams.put("generalComments", comments);
                    varParams.put("varianceReasonId", variance.compareTo(BigDecimal.ZERO) > 0 ? "VAR_FOUND" : "VAR_LOST");
                    varParams.put("quantityOnHandVar", variance);
                    varParams.put("availableToPromiseVar", variance);
                    Map<String, Object> varRes = ctx.runService("createPhysicalInventoryAndVariance", varParams);
                    result.put("inventoryItemId", inventoryItemId);
                    result.put("physicalInventoryId", varRes.get("physicalInventoryId"));
                    result.put("applied", true);
                } else {
                    result.put("applied", false);
                }
                results.add(result);
            }
            Map<String, Object> out = new LinkedHashMap<>();
            out.put("facilityId", facilityId);
            out.put("apply", doApply);
            out.put("rows", results);
            return ResultConverter.toJson(out);
        } catch (GenericEntityException e) {
            throw new McpToolException("Inventory count failed: " + e.getMessage());
        }
    }

    @SuppressWarnings("unchecked")
    private static Map<String, Object> asMap(Object o) throws McpToolException {
        if (!(o instanceof Map)) throw new McpToolException("Expected an object row, got: " + o);
        return (Map<String, Object>) o;
    }

    private static BigDecimal toBigDecimal(Object v) {
        if (v == null) return null;
        if (v instanceof BigDecimal) return (BigDecimal) v;
        if (v instanceof Number) return BigDecimal.valueOf(((Number) v).doubleValue());
        try {
            return new BigDecimal(String.valueOf(v).trim());
        } catch (NumberFormatException e) {
            return null;
        }
    }
}

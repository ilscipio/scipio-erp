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
import java.util.Set;

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
                    description = "Shipments: find, plan, create, update, pack, label, ship."),
            @McpTopic(name = "shipping_rate", title = "Shipping rates", order = 30,
                    description = "Flat shipping rates of the store for each country: list, set, remove.")
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

    /** Longest tracking number that tracking_set stores (ShipmentRouteSegment.trackingIdNumber is a short-varchar of 60 characters). */
    static final int MAX_TRACKING_LENGTH = 60;

    /** Display names of the carrier parties that tracking_set creates when the store has none. */
    private static final Map<String, String> CARRIER_NAMES = Map.of("ROYAL_MAIL", "Royal Mail", "DPD", "DPD", "EVRI", "Evri");

    /** The carrier party id as tracking_set stores it: upper case, letters, digits and underscore only (at most 20); null when it is not valid. */
    static String cleanCarrier(String carrierPartyId) {
        if (carrierPartyId == null) return null;
        String c = carrierPartyId.trim().toUpperCase(java.util.Locale.ROOT);
        return c.matches("[A-Z0-9_]{1,20}") ? c : null;
    }

    /** The name of a new carrier party: the known name, else the id. */
    static String carrierName(String carrier) {
        return CARRIER_NAMES.getOrDefault(carrier, carrier);
    }

    /** The tracking number as the tool stores it: trimmed, without inner spaces; null when it is empty or too long. */
    static String cleanTracking(String trackingIdNumber) {
        if (trackingIdNumber == null) return null;
        String t = trackingIdNumber.trim().replaceAll("\\s+", "");
        return t.isEmpty() || t.length() > MAX_TRACKING_LENGTH ? null : t;
    }

    /**
     * SCIPIO: R4-4: the seller sends a parcel with an own label and enters the tracking number (and the carrier) by hand.
     * The route segment is the first one of the shipment (or the given one); a shipment without a segment gets one.
     */
    @McpTool(topic = "shipment", name = "tracking_set", description = "Set the carrier and the tracking number of a shipment (route segment 00001 unless given). "
            + "A carrier that is not a party in the store is created as a carrier party when the caller may; else it is not stored, and the tracking number is.",
            readOnly = false, destructive = "false", order = 87)
    public static Object setTracking(McpCallContext ctx,
            @McpParam(name = "shipmentId", required = true) String shipmentId,
            @McpParam(name = "trackingIdNumber", description = "The tracking number (at most 60 characters)", required = true) String trackingIdNumber,
            @McpParam(name = "carrierPartyId", description = "The carrier party, e.g. USPS, UPS, FEDEX, DHL", required = false) String carrierPartyId,
            @McpParam(name = "shipmentRouteSegmentId", description = "Default: 00001", required = false) String shipmentRouteSegmentId) throws McpToolException {
        ctx.requirePermission("FACILITY_UPDATE");
        String tracking = cleanTracking(trackingIdNumber);
        if (tracking == null) {
            throw new McpToolException("invalid_tracking: the tracking number is empty or longer than " + MAX_TRACKING_LENGTH + " characters");
        }
        String segId = shipmentRouteSegmentId != null && !shipmentRouteSegmentId.trim().isEmpty() ? shipmentRouteSegmentId.trim() : "00001";
        Delegator delegator = ctx.getDelegator();
        try {
            if (EntityQuery.use(delegator).from("Shipment").where("shipmentId", shipmentId).queryOne() == null) {
                throw new McpToolException("Shipment not found: " + shipmentId);
            }
            String carrier = cleanCarrier(carrierPartyId);
            boolean carrierSet = false;
            if (carrier != null) {
                carrierSet = EntityQuery.use(delegator).from("Party").where("partyId", carrier).queryOne() != null;
                if (!carrierSet) carrierSet = createCarrierParty(ctx, carrier);
            }
            Map<String, Object> params = new LinkedHashMap<>();
            params.put("shipmentId", shipmentId);
            params.put("trackingIdNumber", tracking);
            if (carrierSet) params.put("carrierPartyId", carrier);
            GenericValue segment = EntityQuery.use(delegator).from("ShipmentRouteSegment")
                    .where("shipmentId", shipmentId, "shipmentRouteSegmentId", segId).queryOne();
            if (segment == null) {
                requireCreateForNewSegment(ctx, false);
                Map<String, Object> created = ctx.runService("createShipmentRouteSegment", params);
                Object newId = created == null ? null : created.get("shipmentRouteSegmentId");
                if (newId != null) segId = newId.toString();
            } else {
                params.put("shipmentRouteSegmentId", segId);
                ctx.runService("updateShipmentRouteSegment", params);
            }
            Map<String, Object> out = new LinkedHashMap<>();
            out.put("shipmentId", shipmentId);
            out.put("shipmentRouteSegmentId", segId);
            out.put("trackingIdNumber", tracking);
            out.put("carrierPartyId", carrierSet ? carrier : null);
            out.put("carrierSet", carrierSet);
            return ResultConverter.toJson(out);
        } catch (GenericEntityException e) {
            throw new McpToolException("Setting the tracking number failed: " + e.getMessage());
        }
    }

    /** A new route segment needs FACILITY_CREATE (createShipmentRouteSegment); an existing one needs FACILITY_UPDATE only. */
    static void requireCreateForNewSegment(McpCallContext ctx, boolean segmentExists) throws McpToolException {
        if (!segmentExists) ctx.requirePermission("FACILITY_CREATE");
    }

    /** Creates the carrier party (PartyGroup, role CARRIER) with the caller's permissions; false when the caller may not or a service fails. */
    private static boolean createCarrierParty(McpCallContext ctx, String carrier) {
        try {
            Map<String, Object> group = new LinkedHashMap<>();
            group.put("partyId", carrier);
            group.put("groupName", carrierName(carrier));
            ctx.runService("createPartyGroup", group);
            Map<String, Object> role = new LinkedHashMap<>();
            role.put("partyId", carrier);
            role.put("roleTypeId", "CARRIER");
            ctx.runService("createPartyRole", role);
            return true;
        } catch (McpToolException e) {
            return false;
        }
    }

    // ------------------------------------------------------------------ shipping rates (R4-6)

    /** The carrier party of a flat store rate: no carrier (the store method of the skeleton uses the same party). */
    static final String RATE_CARRIER = "_NA_";
    static final String RATE_STANDARD = "STANDARD";
    static final String RATE_FASTER = "EXPRESS";
    /** The method types of a flat rate: the standard one and one faster option. */
    static final java.util.List<String> RATE_METHODS = java.util.List.of(RATE_STANDARD, RATE_FASTER);
    static final BigDecimal MAX_RATE = new BigDecimal("100000");

    /** One ShipmentCostEstimate of a flat rate, reduced to what the list needs. {@code freeFrom} is set on the estimate of the free threshold only. */
    static final class RateEstimate {
        final String geoId;
        final String method;
        final BigDecimal flat;
        final BigDecimal freeFrom;

        RateEstimate(String geoId, String method, BigDecimal flat, BigDecimal freeFrom) {
            this.geoId = geoId;
            this.method = method;
            this.flat = flat;
            this.freeFrom = freeFrom;
        }
    }

    /** The country code as the tools store it: two letters, upper case; null when it is not valid. */
    static String cleanCountry(String country) {
        if (country == null) return null;
        String c = country.trim().toUpperCase(java.util.Locale.ROOT);
        return c.matches("[A-Z]{2}") ? c : null;
    }

    /** An amount as the rate tools store it: 0 to 100000, two decimals; null when it is missing, negative or too large. */
    static BigDecimal cleanRateAmount(BigDecimal amount) {
        if (amount == null || amount.signum() < 0 || amount.compareTo(MAX_RATE) > 0) return null;
        return amount.setScale(2, RoundingMode.HALF_UP);
    }

    /**
     * The rates of the estimates, one row for each country and method: {country, shipmentMethodTypeId, name, amount, freeFrom}. An estimate with a free
     * threshold sets {@code freeFrom} of the row, an estimate without it sets the {@code amount}. A country that the geo map does not know is left out;
     * a row without a base estimate is left out. The standard method comes first in a country.
     */
    static List<Map<String, Object>> foldRates(List<RateEstimate> estimates, Map<String, String> countryOfGeo, Map<String, String> methodNames) {
        Map<String, Map<String, Object>> rows = new LinkedHashMap<>();
        for (RateEstimate e : estimates) {
            String country = countryOfGeo.get(e.geoId);
            if (country == null || e.method == null) continue;
            Map<String, Object> row = rows.computeIfAbsent(country + "|" + e.method, k -> {
                Map<String, Object> m = new LinkedHashMap<>();
                m.put("country", country);
                m.put("shipmentMethodTypeId", e.method);
                m.put("name", methodNames.getOrDefault(e.method, e.method));
                m.put("amount", null);
                m.put("freeFrom", null);
                return m;
            });
            if (e.freeFrom != null) {
                row.put("freeFrom", e.freeFrom);
            } else {
                row.put("amount", e.flat == null ? BigDecimal.ZERO : e.flat);
            }
        }
        List<Map<String, Object>> out = new ArrayList<>();
        for (Map<String, Object> row : rows.values()) {
            if (row.get("amount") != null) out.add(row);
        }
        out.sort((a, b) -> {
            int c = ((String) a.get("country")).compareTo((String) b.get("country"));
            if (c != 0) return c;
            boolean sa = RATE_STANDARD.equals(a.get("shipmentMethodTypeId"));
            boolean sb = RATE_STANDARD.equals(b.get("shipmentMethodTypeId"));
            return sa == sb ? ((String) a.get("shipmentMethodTypeId")).compareTo((String) b.get("shipmentMethodTypeId")) : (sa ? -1 : 1);
        });
        return out;
    }

    private static String rateStore(McpCallContext ctx, String productStoreId) throws McpToolException, GenericEntityException {
        if (productStoreId != null && !productStoreId.trim().isEmpty()) return productStoreId.trim();
        if (ctx.getProductStoreId() != null) return ctx.getProductStoreId();
        List<GenericValue> stores = EntityQuery.use(ctx.getDelegator()).from("ProductStore").maxRows(2).queryList();
        if (stores.size() != 1) throw new McpToolException("productStoreId is required (the server has no single product store)");
        return stores.get(0).getString("productStoreId");
    }

    private static String geoOfCountry(Delegator delegator, String country) throws GenericEntityException {
        GenericValue geo = EntityQuery.use(delegator).from("Geo").where("geoTypeId", "COUNTRY", "geoCode", country).cache().queryFirst();
        return geo == null ? null : geo.getString("geoId");
    }

    private static List<GenericValue> rateEstimates(Delegator delegator, String storeId, String method, String geoId) throws GenericEntityException {
        List<EntityCondition> conds = new ArrayList<>();
        conds.add(EntityCondition.makeCondition("productStoreId", storeId));
        conds.add(EntityCondition.makeCondition("carrierPartyId", RATE_CARRIER));
        conds.add(EntityCondition.makeCondition("carrierRoleTypeId", "CARRIER"));
        if (method != null) conds.add(EntityCondition.makeCondition("shipmentMethodTypeId", method));
        if (geoId != null) conds.add(EntityCondition.makeCondition("geoIdTo", geoId));
        return EntityQuery.use(delegator).from("ShipmentCostEstimate").where(conds).orderBy("shipmentCostEstimateId").queryList();
    }

    private static BigDecimal breakFrom(Delegator delegator, GenericValue estimate) throws GenericEntityException {
        String id = estimate.getString("priceBreakId");
        if (id == null || id.isEmpty()) return null;
        GenericValue b = EntityQuery.use(delegator).from("QuantityBreak").where("quantityBreakId", id).queryOne();
        return b == null ? null : b.getBigDecimal("fromQuantity");
    }

    @McpTool(topic = "shipping_rate", name = "list", description = "List the flat shipping rates of the store: for each country and method the amount and the free-from threshold. "
            + "The checkout charges the rate of the country of the ship-to address.", readOnly = true, order = 10)
    public static Object listShippingRates(McpCallContext ctx,
            @McpParam(name = "productStoreId", description = "Default: the product store of the server", required = false) String productStoreId) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        try {
            String storeId = rateStore(ctx, productStoreId);
            List<RateEstimate> estimates = new ArrayList<>();
            Map<String, String> countryOfGeo = new HashMap<>();
            for (GenericValue e : rateEstimates(delegator, storeId, null, null)) {
                String geoId = e.getString("geoIdTo");
                if (geoId == null || geoId.isEmpty()) continue;
                if (!countryOfGeo.containsKey(geoId)) {
                    GenericValue geo = EntityQuery.use(delegator).from("Geo").where("geoId", geoId).cache().queryOne();
                    String code = geo == null || !"COUNTRY".equals(geo.getString("geoTypeId")) ? null : cleanCountry(geo.getString("geoCode"));
                    countryOfGeo.put(geoId, code);
                }
                String methodType = e.getString("shipmentMethodTypeId");
                BigDecimal from = breakFrom(delegator, e);
                if (e.getString("priceBreakId") != null && !e.getString("priceBreakId").isEmpty() && from == null) continue;
                estimates.add(new RateEstimate(geoId, methodType, e.getBigDecimal("orderFlatPrice"), from));
            }
            Map<String, String> names = new HashMap<>();
            for (String m : RATE_METHODS) {
                GenericValue t = EntityQuery.use(delegator).from("ShipmentMethodType").where("shipmentMethodTypeId", m).cache().queryOne();
                names.put(m, t != null && t.getString("description") != null ? t.getString("description") : m);
            }
            String currency = storeCurrency(delegator, storeId, ctx.getCurrencyUomId());
            Map<String, Object> out = new LinkedHashMap<>();
            out.put("productStoreId", storeId);
            out.put("currencyUomId", currency);
            out.put("rates", foldRates(estimates, countryOfGeo, names));
            return ResultConverter.toJson(out);
        } catch (GenericEntityException e) {
            throw new McpToolException("Shipping rate lookup failed: " + e.getMessage());
        }
    }

    /**
     * SCIPIO: R4-6: the seller sets a flat rate for one country: a ShipmentCostEstimate of the store method with {@code geoIdTo} = the country and
     * {@code orderFlatPrice} = the amount. A free-from threshold is a second estimate of the same country with a price break (from the threshold, no
     * upper limit) and the flat price 0; the core estimate service takes the estimate with the price break when the order total is in it, so the rate is
     * the one that the buyer pays at checkout. A missing store method (the faster option) is created with its method type.
     */
    @McpTool(topic = "shipping_rate", name = "set", description = "Set the flat shipping rate of one country and method (standard, or EXPRESS as the faster option). "
            + "freeFrom: the order total from which shipping is free; omit it for no free shipping. The call replaces the rate of the country and method.",
            readOnly = false, destructive = "false", order = 20)
    public static Object setShippingRate(McpCallContext ctx,
            @McpParam(name = "country", description = "ISO 3166 alpha-2 code, e.g. GB", required = true) String country,
            @McpParam(name = "amount", description = "What the buyer pays, in the currency of the store (0 to 100000)", required = true) BigDecimal amount,
            @McpParam(name = "freeFrom", description = "Shipping is free from this order total; omit or 0 for none", required = false) BigDecimal freeFrom,
            @McpParam(name = "shipmentMethodTypeId", description = "STANDARD (default) or EXPRESS", required = false) String shipmentMethodTypeId,
            @McpParam(name = "productStoreId", description = "Default: the product store of the server", required = false) String productStoreId) throws McpToolException {
        ctx.requirePermission("FACILITY_UPDATE");
        String code = cleanCountry(country);
        if (code == null) throw new McpToolException("invalid_country: send the two-letter country code");
        BigDecimal flat = cleanRateAmount(amount);
        if (flat == null) throw new McpToolException("invalid_amount: the rate is a number from 0 to " + MAX_RATE.toPlainString());
        BigDecimal threshold = null;
        if (freeFrom != null && freeFrom.signum() != 0) {
            threshold = cleanRateAmount(freeFrom);
            if (threshold == null || threshold.signum() == 0) throw new McpToolException("invalid_free_from: the free-from total is a number above 0 and up to " + MAX_RATE.toPlainString());
        }
        String method = shipmentMethodTypeId == null || shipmentMethodTypeId.trim().isEmpty() ? RATE_STANDARD : shipmentMethodTypeId.trim().toUpperCase(java.util.Locale.ROOT);
        if (!RATE_METHODS.contains(method)) throw new McpToolException("invalid_method: use STANDARD or EXPRESS");
        Delegator delegator = ctx.getDelegator();
        try {
            String storeId = rateStore(ctx, productStoreId);
            String geoId = geoOfCountry(delegator, code);
            if (geoId == null) throw new McpToolException("invalid_country: the country " + code + " is not known");
            String methId = ensureRateMethod(ctx, storeId, method);
            List<GenericValue> bases = new ArrayList<>();
            List<GenericValue> frees = new ArrayList<>();
            splitEstimates(rateEstimates(delegator, storeId, method, geoId), bases, frees);
            GenericValue base = bases.isEmpty() ? null : bases.get(0);
            GenericValue free = frees.isEmpty() ? null : frees.get(0);
            // exactly one base estimate and at most one free-from estimate for each country and method: the others (duplicates) go
            for (int i = 1; i < bases.size(); i++) removeEstimate(ctx, bases.get(i), null);
            String keepBreak = free == null ? null : free.getString("priceBreakId");
            for (int i = 1; i < frees.size(); i++) removeEstimate(ctx, frees.get(i), keepBreak);
            writeEstimate(ctx, base, storeId, methId, method, geoId, flat, null);
            if (threshold == null) {
                removeEstimate(ctx, free, null);
            } else {
                String breakId = keepBreak;
                Map<String, Object> bp = new LinkedHashMap<>();
                bp.put("fromQuantity", threshold);
                bp.put("thruQuantity", BigDecimal.ZERO);
                if (breakId != null && EntityQuery.use(delegator).from("QuantityBreak").where("quantityBreakId", breakId).queryOne() != null) {
                    bp.put("quantityBreakId", breakId);
                    ctx.runService("updateQuantityBreak", bp);
                } else {
                    Set<String> before = breakIds(delegator, threshold);
                    ctx.runService("createQuantityBreak", bp);
                    breakId = newId(before, breakIds(delegator, threshold));
                    if (breakId == null) throw new McpToolException("The price break of the free-from total was not created");
                }
                writeEstimate(ctx, free, storeId, methId, method, geoId, BigDecimal.ZERO, breakId);
            }
            Map<String, Object> out = new LinkedHashMap<>();
            out.put("productStoreId", storeId);
            out.put("country", code);
            out.put("shipmentMethodTypeId", method);
            out.put("amount", flat);
            out.put("freeFrom", threshold);
            out.put("currencyUomId", storeCurrency(delegator, storeId, ctx.getCurrencyUomId()));
            return ResultConverter.toJson(out);
        } catch (GenericEntityException e) {
            throw new McpToolException("Setting the shipping rate failed: " + e.getMessage());
        }
    }

    @McpTool(topic = "shipping_rate", name = "remove", description = "Remove the flat shipping rate of one country and method (for example the faster option).",
            readOnly = false, destructive = "true", requiresConfirmation = true, order = 30)
    public static Object removeShippingRate(McpCallContext ctx,
            @McpParam(name = "country", description = "ISO 3166 alpha-2 code, e.g. GB", required = true) String country,
            @McpParam(name = "shipmentMethodTypeId", description = "STANDARD or EXPRESS (default EXPRESS)", required = false) String shipmentMethodTypeId,
            @McpParam(name = "productStoreId", required = false) String productStoreId) throws McpToolException {
        ctx.requirePermission("FACILITY_UPDATE");
        String code = cleanCountry(country);
        if (code == null) throw new McpToolException("invalid_country: send the two-letter country code");
        String method = shipmentMethodTypeId == null || shipmentMethodTypeId.trim().isEmpty() ? RATE_FASTER : shipmentMethodTypeId.trim().toUpperCase(java.util.Locale.ROOT);
        if (!RATE_METHODS.contains(method)) throw new McpToolException("invalid_method: use STANDARD or EXPRESS");
        Delegator delegator = ctx.getDelegator();
        try {
            String storeId = rateStore(ctx, productStoreId);
            String geoId = geoOfCountry(delegator, code);
            int removed = 0;
            if (geoId != null) {
                for (GenericValue e : rateEstimates(delegator, storeId, method, geoId)) {
                    removeEstimate(ctx, e, null);
                    removed++;
                }
            }
            Map<String, Object> out = new LinkedHashMap<>();
            out.put("country", code);
            out.put("shipmentMethodTypeId", method);
            out.put("removed", removed > 0);
            return ResultConverter.toJson(out);
        } catch (GenericEntityException e) {
            throw new McpToolException("Removing the shipping rate failed: " + e.getMessage());
        }
    }

    /** The id of the store method of the rate (partyId _NA_); a missing method is created, with its method type and carrier method (the faster option). */
    private static String ensureRateMethod(McpCallContext ctx, String storeId, String method) throws McpToolException, GenericEntityException {
        Delegator delegator = ctx.getDelegator();
        GenericValue meth = EntityQuery.use(delegator).from("ProductStoreShipmentMeth")
                .where("productStoreId", storeId, "shipmentMethodTypeId", method, "partyId", RATE_CARRIER, "roleTypeId", "CARRIER").queryFirst();
        if (meth != null) return meth.getString("productStoreShipMethId");
        if (EntityQuery.use(delegator).from("ShipmentMethodType").where("shipmentMethodTypeId", method).queryOne() == null) {
            Map<String, Object> t = new LinkedHashMap<>();
            t.put("shipmentMethodTypeId", method);
            t.put("description", RATE_FASTER.equals(method) ? "Faster delivery" : "Standard");
            ctx.runService("createShipmentMethodType", t);
        }
        if (EntityQuery.use(delegator).from("CarrierShipmentMethod")
                .where("shipmentMethodTypeId", method, "partyId", RATE_CARRIER, "roleTypeId", "CARRIER").queryOne() == null) {
            Map<String, Object> c = new LinkedHashMap<>();
            c.put("shipmentMethodTypeId", method);
            c.put("partyId", RATE_CARRIER);
            c.put("roleTypeId", "CARRIER");
            c.put("sequenceNumber", RATE_FASTER.equals(method) ? 10L : 5L);
            ctx.runService("createCarrierShipmentMethod", c);
        }
        Map<String, Object> params = new LinkedHashMap<>();
        params.put("productStoreId", storeId);
        params.put("shipmentMethodTypeId", method);
        params.put("partyId", RATE_CARRIER);
        params.put("roleTypeId", "CARRIER");
        params.put("includeNoChargeItems", "Y");
        params.put("allowUspsAddr", "Y");
        params.put("requireUspsAddr", "N");
        params.put("sequenceNumber", RATE_FASTER.equals(method) ? 2L : 1L);
        Map<String, Object> res = ctx.runService("createProductStoreShipMeth", params);
        Object id = res == null ? null : res.get("productStoreShipMethId");
        if (id == null) throw new McpToolException("The store method " + method + " was not created");
        return id.toString();
    }

    private static void writeEstimate(McpCallContext ctx, GenericValue existing, String storeId, String methId, String method, String geoId,
            BigDecimal flat, String breakId) throws McpToolException {
        Map<String, Object> params = new LinkedHashMap<>();
        params.put("orderFlatPrice", flat);
        if (existing != null) {
            params.put("shipmentCostEstimateId", existing.getString("shipmentCostEstimateId"));
            params.put("productStoreShipMethId", methId);
            if (breakId != null) params.put("priceBreakId", breakId);
            ctx.runService("updateShipmentCostEstimate", params);
        } else {
            params.put("shipmentMethodTypeId", method);
            params.put("carrierPartyId", RATE_CARRIER);
            params.put("carrierRoleTypeId", "CARRIER");
            params.put("productStoreShipMethId", methId);
            params.put("productStoreId", storeId);
            params.put("geoIdTo", geoId);
            if (breakId != null) params.put("priceBreakId", breakId);
            ctx.runService("createShipmentCostEstimate", params);
        }
    }

    /** Removes an estimate and its price break (not the break {@code keepBreakId}); null is nothing to remove. */
    private static void removeEstimate(McpCallContext ctx, GenericValue estimate, String keepBreakId) throws McpToolException, GenericEntityException {
        if (estimate == null) return;
        String breakId = estimate.getString("priceBreakId");
        Map<String, Object> params = new LinkedHashMap<>();
        params.put("shipmentCostEstimateId", estimate.getString("shipmentCostEstimateId"));
        ctx.runService("deleteShipmentCostEstimate", params);
        if (breakId != null && !breakId.isEmpty() && !breakId.equals(keepBreakId)
                && EntityQuery.use(ctx.getDelegator()).from("QuantityBreak").where("quantityBreakId", breakId).queryOne() != null) {
            Map<String, Object> bp = new LinkedHashMap<>();
            bp.put("quantityBreakId", breakId);
            ctx.runService("deleteQuantityBreak", bp);
        }
    }

    /** Splits the estimates (in the given order) into the base estimates (no price break) and the free-from estimates (with a price break). */
    static void splitEstimates(List<GenericValue> estimates, List<GenericValue> bases, List<GenericValue> frees) {
        for (GenericValue e : estimates) {
            String id = e.getString("priceBreakId");
            if (id != null && !id.isEmpty()) frees.add(e);
            else bases.add(e);
        }
    }

    /** The ids of the price breaks from the threshold with no upper limit. */
    private static Set<String> breakIds(Delegator delegator, BigDecimal threshold) throws GenericEntityException {
        Set<String> ids = new java.util.HashSet<>();
        for (GenericValue b : EntityQuery.use(delegator).from("QuantityBreak").where("fromQuantity", threshold, "thruQuantity", BigDecimal.ZERO).queryList()) {
            ids.add(b.getString("quantityBreakId"));
        }
        return ids;
    }

    /** The id that is in {@code after} and not in {@code before} (the break that a service call created); null when there is none. */
    static String newId(Set<String> before, Set<String> after) {
        for (String id : after) {
            if (!before.contains(id)) return id;
        }
        return null;
    }

    /** The currency of the store; the fallback when the store has none. */
    private static String storeCurrency(Delegator delegator, String storeId, String fallback) throws GenericEntityException {
        GenericValue store = EntityQuery.use(delegator).from("ProductStore").where("productStoreId", storeId).cache().queryOne();
        return currencyOf(store == null ? null : store.getString("defaultCurrencyUomId"), fallback);
    }

    static String currencyOf(String storeCurrency, String fallback) {
        return storeCurrency != null && !storeCurrency.isEmpty() ? storeCurrency : fallback;
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

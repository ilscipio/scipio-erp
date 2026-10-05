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
package com.ilscipio.scipio.product.event;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://product/script/org/ofbiz/shipment/issuance/IssuanceServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class IssuanceServices {

    private static final String MODULE = IssuanceServices.class.getName();


    /**
     * Create ItemIssuance
     */
    public static Map<String, Object> createItemIssuance(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue product = null;
        Map<String, Object> updateContext = null;
        Boolean affectAccounting = null;
        Object operationName = "Create ItemIssuance";
        // TODO: Call simple-method "checkCanChangeShipmentStatusPacked" from "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml"
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue newEntity = delegator.makeValue("ItemIssuance");
        ((GenericValue) newEntity).put("itemIssuanceId", delegator.getNextSeqId("ItemIssuance"));
        result.put("itemIssuanceId", newEntity.get("itemIssuanceId"));
        newEntity.setNonPKFields(context);
        if (UtilValidate.isEmpty(newEntity.get("issuedDateTime"))) {
            Timestamp newEntity_issuedDateTime = new Timestamp(System.currentTimeMillis());
        }
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        affectAccounting = Boolean.TRUE;
        GenericValue inventoryItem = null;
        try {
            inventoryItem = newEntity.getRelatedOne("InventoryItem", false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related one InventoryItem: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(inventoryItem)) {
            if ("SERIALIZED_INV_ITEM".equals(inventoryItem.get("inventoryItemTypeId"))) {
                updateContext.put("inventoryItemId", inventoryItem.get("inventoryItemId"));
                updateContext.put("statusId", "INV_DELIVERED");
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("updateInventoryItem", updateContext);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling updateInventoryItem: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                try {
                    product = EntityQuery.use(delegator)
                            .from("Product")
                            .where(UtilMisc.toMap("productId", inventoryItem.get("productId")))
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying Product: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (("SERVICE_PRODUCT".equals(product.get("productTypeId")) || "ASSET_USAGE_OUT_IN".equals(product.get("productTypeId")) || "AGGREGATEDSERV_CONF".equals(product.get("productTypeId")))) {
                    affectAccounting = Boolean.FALSE;
                }
            }
        }
        result.put("affectAccounting", affectAccounting);

        return result;
    }


    /**
     * Update ItemIssuance
     */
    public static Map<String, Object> updateItemIssuance(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Object operationName = "Update ItemIssuance";
        // TODO: Call simple-method "checkCanChangeShipmentStatusPacked" from "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml"
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ItemIssuance")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ItemIssuance: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        lookedUpValue.setNonPKFields(context);
        try {
            delegator.store(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Delete ItemIssuance
     */
    public static Map<String, Object> deleteItemIssuance(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Object operationName = "Delete ItemIssuance";
        // TODO: Call simple-method "checkCanChangeShipmentStatusPacked" from "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml"
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ItemIssuance")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ItemIssuance: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            delegator.removeValue(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Create ItemIssuanceRole
     */
    public static Map<String, Object> createItemIssuanceRole(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Object operationName = "Create ItemIssuanceRole";
        // TODO: Call simple-method "checkCanChangeShipmentStatusPacked" from "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml"
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue newEntity = delegator.makeValue("ItemIssuanceRole");
        newEntity.setPKFields(context);
        newEntity.setNonPKFields(context);
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Delete ItemIssuanceRole
     */
    public static Map<String, Object> deleteItemIssuanceRole(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Object operationName = "Delete ItemIssuanceRole";
        // TODO: Call simple-method "checkCanChangeShipmentStatusPacked" from "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml"
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ItemIssuanceRole")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ItemIssuanceRole: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            delegator.removeValue(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Issue OrderItem to Shipment
     */
    public static Map<String, Object> issueOrderItemToShipment(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Map<String, Object> shipmentItemLookupPk = null;
        Map<String, Object> shipmentItemCreate = null;
        GenericValue shipmentItem = null;
        List<GenericValue> shipmentItems = null;
        Map<String, Object> inlineResult = null;
        Object operationName = "Issue OrderItem to Shipment";
        // TODO: Call simple-method "checkCanChangeShipmentStatusPacked" from "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml"
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue orderHeader = null;
        try {
            orderHeader = EntityQuery.use(delegator)
                    .from("OrderHeader")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderHeader: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if ("SALES_ORDER".equals(orderHeader.get("orderTypeId"))) {
            error_list.add("Not issuing Order Item to shipment [" + context.get("shipmentId") + "] because the order is a Sales Order for order [" + orderHeader.get("orderId") + "] order item [" + context.get("orderItemSeqId") + "] (should call the issueOrderItemShipGrpInvResToShipment service)");
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue orderItem = null;
        try {
            orderItem = EntityQuery.use(delegator)
                    .from("OrderItem")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderItem: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue orderItemShipGroupAssoc = null;
        try {
            orderItemShipGroupAssoc = EntityQuery.use(delegator)
                    .from("OrderItemShipGroupAssoc")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderItemShipGroupAssoc: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue shipment = null;
        try {
            shipment = EntityQuery.use(delegator)
                    .from("Shipment")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Shipment: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        inlineResult = findCreateIssueShipmentItem(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }

        return result;
    }


    /**
     * Issue OrderItemShipGrpInvRes to Shipment
     */
    public static Map<String, Object> issueOrderItemShipGrpInvResToShipment(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue shipmentItemLookupPk = null;
        Object originalQuantity = null;
        GenericValue shipmentItem = null;
        Map<String, Object> inlineResult = null;
        GenericValue statusItemSent = null;
        Map<String, Object> changeOrderItemStatusMap = null;
        List<GenericValue> statusValidChangeItemSent = null;
        GenericValue oisg = null;
        List<GenericValue> otherOiirs = null;
        List<GenericValue> itemIssuances = null;
        Object qtyForShipmentItem = null;
        GenericValue itemIssuance = null;
        Object orderShipmentAmount = null;
        Object otherInventoryItemQuantity = null;
        Map<String, Object> shipmentItemCreate = null;
        List<GenericValue> shipmentItems = null;
        Object itemIssuanceId = null;
        Map<String, Object> itemIssuanceCreate = null;
        Object userLoginId = null;
        GenericValue checkPartyRole = null;
        GenericValue partyRole = null;
        Map<String, Object> itemIssuanceRoleCreate = null;
        GenericValue itemIssuanceRole = null;
        Object operationName = "Issue OrderItemShipGrpInvRes to Shipment";
        // TODO: Call simple-method "checkCanChangeShipmentStatusPacked" from "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml"
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue OrderItemShipGrpInvResLookupPk = delegator.makeValue("OrderItemShipGrpInvRes");
        OrderItemShipGrpInvResLookupPk.setPKFields(context);
        GenericValue orderItemShipGrpInvRes = null;
        try {
            orderItemShipGrpInvRes = EntityQuery.use(delegator)
                    .from(OrderItemShipGrpInvResLookupPk.getEntityName())
                    .where(OrderItemShipGrpInvResLookupPk)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key : " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Debug.logInfo("order item ship grp inv res info: " + orderItemShipGrpInvRes, MODULE);
        GenericValue orderHeaderLookupPk = delegator.makeValue("OrderHeader");
        orderHeaderLookupPk.setPKFields(context);
        GenericValue orderHeader = null;
        try {
            orderHeader = EntityQuery.use(delegator)
                    .from(orderHeaderLookupPk.getEntityName())
                    .where(orderHeaderLookupPk)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key : " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (!"SALES_ORDER".equals(orderHeader.get("orderTypeId"))) {
            error_list.add("Not issuing Order Item Ship Group Inventory Reservation to shipment [" + context.get("shipmentId") + "] because the order is not a Sales Order for order [" + orderItemShipGrpInvRes.get("orderId") + "] order item [" + orderItemShipGrpInvRes.get("orderItemSeqId") + "] inventoryItem [" + orderItemShipGrpInvRes.get("inventoryItemId") + "] (should call the issueOrderItemToShipment service)");
        }
        if (UtilValidate.isEmpty(context.get("quantity"))) {
            {
                String errorMsg = UtilProperties.getMessage("ProductUiLabels", "ProductNotIssueOrderToShipment", locale);
                error_list.add(errorMsg);
            }
        }
        if (UtilValidate.isEmpty(orderItemShipGrpInvRes.get("quantity"))) {
            Debug.logInfo("Order item reservation amount is null! PK lookup: " + OrderItemShipGrpInvResLookupPk, MODULE);
        }
        if (((Comparable) context.get("quantity")).compareTo(BigDecimal.ZERO) <= 0) {
            {
                String errorMsg = UtilProperties.getMessage("ProductUiLabels", "ProductNotIssueOrderToShipmentQuantityLess", locale);
                error_list.add(errorMsg);
            }
        }
        if (context.get("quantity") != null /* TODO: field compare operator greater */) {
            {
                String errorMsg = UtilProperties.getMessage("ProductUiLabels", "ProductNotIssueOrderToShipmentQuantityGreater", locale);
                error_list.add(errorMsg);
            }
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        Debug.logInfo("orderId: " + context.get("orderId"), MODULE);
        try {
            orderHeader = EntityQuery.use(delegator)
                    .from("OrderHeader")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderHeader: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue orderItem = null;
        try {
            orderItem = EntityQuery.use(delegator)
                    .from("OrderItem")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderItem: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue product = null;
        try {
            product = EntityQuery.use(delegator)
                    .from("Product")
                    .where(UtilMisc.toMap("productId", orderItem.get("productId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Product: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Debug.logInfo("orderHeader: " + orderHeader, MODULE);
        Debug.logInfo("orderItem: " + orderItem, MODULE);
        Debug.logInfo("product: " + product, MODULE);
        GenericValue inventoryItem = null;
        try {
            inventoryItem = EntityQuery.use(delegator)
                    .from("InventoryItem")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying InventoryItem: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue shipment = null;
        try {
            shipment = EntityQuery.use(delegator)
                    .from("Shipment")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Shipment: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue orderShipmentLookupPk = delegator.makeValue("OrderShipment");
        orderShipmentLookupPk.setPKFields(context);
        List<GenericValue> orderShipments = null;
        try {
            orderShipments = EntityQuery.use(delegator)
                    .from("OrderShipment")
                    .where(orderShipmentLookupPk)
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderShipment: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue orderShipment = EntityUtil.getFirst((List<GenericValue>) orderShipments);
        context.put("itemIssuances", itemIssuances);
        context.put("orderShipment", orderShipment);
        context.put("qtyForShipmentItem", qtyForShipmentItem);
        context.put("itemIssuance", itemIssuance);
        context.put("orderShipmentAmount", orderShipmentAmount);
        context.put("otherInventoryItemQuantity", otherInventoryItemQuantity);
        calcQtyForShipmentItemInline(dctx, context);
        Debug.logInfo("qtyForShipmentItem: " + qtyForShipmentItem, MODULE);
        Debug.logInfo("shipment: " + shipment, MODULE);
        if (((Comparable) qtyForShipmentItem).compareTo(BigDecimal.ZERO) >= 0) {
            if (UtilValidate.isNotEmpty(orderShipment)) {
                result.put("shipmentItemSeqId", orderShipment.get("shipmentItemSeqId"));
                shipmentItemLookupPk = delegator.makeValue("ShipmentItem");
                shipmentItemLookupPk.setPKFields(context);
                shipmentItemLookupPk.put("shipmentItemSeqId", orderShipment.get("shipmentItemSeqId"));
                try {
                    shipmentItem = EntityQuery.use(delegator)
                            .from(shipmentItemLookupPk.getEntityName())
                            .where(shipmentItemLookupPk)
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error finding by primary key : " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
            if (!java.util.Objects.equals(qtyForShipmentItem, BigDecimal.ZERO)) {
                originalQuantity = context.get("quantity");
                context.put("quantity", qtyForShipmentItem);
                inlineResult = findCreateIssueShipmentItem(dctx, context);
                if (ServiceUtil.isError(inlineResult)) {
                    return inlineResult;
                }
                context.put("quantity", originalQuantity);
            }
        } else {
            orderShipment.set("quantity", new BigDecimal(context.get("quantity").toString()));
            try {
                delegator.store(orderShipment);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            result.put("shipmentItemSeqId", orderShipment.get("shipmentItemSeqId"));
            shipmentItemLookupPk = delegator.makeValue("ShipmentItem");
            shipmentItemLookupPk.setPKFields(context);
            shipmentItemLookupPk.put("shipmentItemSeqId", orderShipment.get("shipmentItemSeqId"));
            try {
                shipmentItem = EntityQuery.use(delegator)
                        .from(shipmentItemLookupPk.getEntityName())
                        .where(shipmentItemLookupPk)
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error finding by primary key : " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        Object eventDate = context.get("eventDate");
        Object shipmentId = context.get("shipmentId");
        inlineResult = findCreateItemIssuance(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        inlineResult = associateIssueRoles(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        orderItemShipGrpInvRes.set("quantity", (new BigDecimal(orderItemShipGrpInvRes.get("quantity").toString())).subtract(new BigDecimal(context.get("quantity").toString())));
        if (java.util.Objects.equals(orderItemShipGrpInvRes.get("quantity"), BigDecimal.ZERO)) {
            try {
                delegator.removeValue(orderItemShipGrpInvRes);
            } catch (Exception e) {
                Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (!"SHIPMENT_SCHEDULED".equals(shipment.get("statusId"))) {
                try {
                    otherOiirs = orderItem.getRelated("OrderItemShipGrpInvRes", null, null, false);
                } catch (Exception e) {
                    Debug.logError(e, "Error getting related OrderItemShipGrpInvRes: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (UtilValidate.isEmpty(otherOiirs)) {
                    try {
                        oisg = orderItemShipGrpInvRes.getRelatedOne("OrderItemShipGroup", false);
                    } catch (Exception e) {
                        Debug.logError(e, "Error getting related one OrderItemShipGroup: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    try {
                        statusItemSent = EntityQuery.use(delegator)
                                .from("StatusItem")
                                .where(UtilMisc.toMap("statusId", "ITEM_SENT"))
                                .queryOne();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying StatusItem: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    try {
                        statusValidChangeItemSent = statusItemSent.getRelated("ToStatusValidChange", null, null, false);
                    } catch (Exception e) {
                        Debug.logError(e, "Error getting related ToStatusValidChange: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    Object changeOrderItemStatusMap_statusId = null;
                    if (("POS_SALES_CHANNEL".equals(orderHeader.get("salesChannelEnumId")) || "DIGITAL_GOOD".equals(product.get("productTypeId")) || "NO_SHIPPING".equals(oisg.get("shipmentMethodTypeId")) || (UtilValidate.isEmpty(statusItemSent) && UtilValidate.isEmpty(statusValidChangeItemSent)))) {
                        changeOrderItemStatusMap.put("statusId", "ITEM_COMPLETED");
                        Debug.logInfo("OrderItem [" + orderItem.get("productId") + "] ITEM_COMPLETED", MODULE);
                    } else {
                        changeOrderItemStatusMap.put("statusId", "ITEM_SENT");
                        Debug.logInfo("OrderItem [" + orderItem.get("productId") + "] ITEM_SENT", MODULE);
                    }
                    changeOrderItemStatusMap.put("orderId", orderItem.get("orderId"));
                    changeOrderItemStatusMap.put("orderItemSeqId", orderItem.get("orderItemSeqId"));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("changeOrderItemStatus", changeOrderItemStatusMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling changeOrderItemStatus: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            } else {
                Debug.logInfo("orderId: " + orderItem.get("orderId") + " orderItemSeqId: " + orderItem.get("orderItemSeqId"), MODULE);
                Debug.logInfo("Items issued but can't set order item status to ITEM_COMPLETED because shipment status is SHIPMENT_SCHEDULED", MODULE);
            }
        } else {
            try {
                delegator.store(orderItemShipGrpInvRes);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        Map<String, Object> createDetailMap = new HashMap<String, Object>();
        createDetailMap.put("inventoryItemId", inventoryItem.get("inventoryItemId"));
        createDetailMap.put("orderId", orderItem.get("orderId"));
        createDetailMap.put("orderItemSeqId", orderItem.get("orderItemSeqId"));
        createDetailMap.put("shipGroupSeqId", orderItemShipGrpInvRes.get("shipGroupSeqId"));
        createDetailMap.put("shipmentId", shipmentItem.get("shipmentId"));
        createDetailMap.put("shipmentItemSeqId", shipmentItem.get("shipmentItemSeqId"));
        createDetailMap.put("itemIssuanceId", itemIssuanceId);
        ((Map<String, Object>) createDetailMap).put("quantityOnHandDiff", new BigDecimal(context.get("quantity").toString()));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createInventoryItemDetail", createDetailMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createInventoryItemDetail: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        createDetailMap = new HashMap<String, Object>();
        List<GenericValue> oisgirs = null;
        try {
            oisgirs = EntityQuery.use(delegator)
                    .from("OrderItemShipGrpInvRes")
                    .where(UtilMisc.toMap("orderId", orderItemShipGrpInvRes.get("orderId"), "orderItemSeqId", orderItemShipGrpInvRes.get("orderItemSeqId"), "inventoryItemId", orderItemShipGrpInvRes.get("inventoryItemId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Calculate quantity for a shipment item - meant to be called in-line
     */
    public static Map<String, Object> calcQtyForShipmentItemInline(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<GenericValue> itemIssuances = null;
        Object otherInventoryItemQuantity = null;
        if (UtilValidate.isNotEmpty(context.get("inventoryItemId"))) {
            try {
                itemIssuances = EntityQuery.use(delegator)
                        .from("ItemIssuance")
                        .where(UtilMisc.toMap("orderId", context.get("orderId"), "orderItemSeqId", context.get("orderItemSeqId"), "shipGroupSeqId", context.get("shipGroupSeqId"), "shipmentId", context.get("shipmentId")))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            otherInventoryItemQuantity = "0";
            if (itemIssuances != null) {
                for (GenericValue itemIssuance : itemIssuances) {
                    if (!java.util.Objects.equals(itemIssuance.get("inventoryItemId"), context.get("inventoryItemId"))) {
                        otherInventoryItemQuantity = new BigDecimal(itemIssuance.get("quantity").toString());
                    }
                }
            }
        }
        Object orderShipmentAmount = new BigDecimal(otherInventoryItemQuantity.toString());
        Object qtyForShipmentItem = new BigDecimal(orderShipmentAmount.toString());

        return result;
    }


    /**
     * Find or Create ShipmentItem to Issue To - meant to be called in-line
     */
    public static Map<String, Object> findCreateIssueShipmentItem(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue shipmentItem = null;
        List<GenericValue> shipmentItems = null;
        Map<String, Object> shipmentItemLookupPk = null;
        Map<String, Object> shipmentItemCreate = null;
        GenericValue orderShipment = null;
        Map<String, Object> orderShipmentCreate = null;
        GenericValue orderShipmentLookupPk = null;
        List<GenericValue> orderShipments = null;
        Map<String, Object> orderItem = new HashMap<String, Object>();
        if (UtilValidate.isNotEmpty(((Map<String, Object>) orderItem).get("productId"))) {
            try {
                shipmentItems = EntityQuery.use(delegator)
                        .from("ShipmentItem")
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ShipmentItem: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            shipmentItem = EntityUtil.getFirst((List<GenericValue>) shipmentItems);
        }
        if (UtilValidate.isEmpty(shipmentItem)) {
            shipmentItemCreate.put("productId", ((Map<String, Object>) orderItem).get("productId"));
            shipmentItemCreate.put("shipmentId", context.get("shipmentId"));
            shipmentItemCreate.put("quantity", context.get("quantity"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createShipmentItem", shipmentItemCreate);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
                shipmentItemLookupPk.put("shipmentItemSeqId", serviceResult.get("shipmentItemSeqId"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling createShipmentItem: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            shipmentItemLookupPk.put("shipmentId", context.get("shipmentId"));
            try {
                shipmentItem = EntityQuery.use(delegator)
                        .from("ShipmentItem")
                        .where(shipmentItemLookupPk)
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error finding by primary key ShipmentItem: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        } else {
            shipmentItem.set("quantity", new BigDecimal(context.get("quantity").toString()));
            try {
                delegator.store(shipmentItem);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        context.put("orderShipment", orderShipment);
        context.put("orderShipmentCreate", orderShipmentCreate);
        context.put("orderShipmentLookupPk", orderShipmentLookupPk);
        context.put("orderItem", orderItem);
        context.put("orderShipments", orderShipments);
        context.put("shipmentItem", shipmentItem);
        createOrUpdateOrderShipmentInline(dctx, context);
        result.put("shipmentItemSeqId", shipmentItem.get("shipmentItemSeqId"));

        return result;
    }


    /**
     * Create or update the OrderShipment - meant to be called in-line
     */
    public static Map<String, Object> createOrUpdateOrderShipmentInline(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> orderShipmentCreate = null;
        orderShipmentCreate.put("shipmentId", context.get("shipmentId"));
        orderShipmentCreate.put("shipmentItemSeqId", ((Map<String, Object>) context.get("shipmentItem")).get("shipmentItemSeqId"));
        orderShipmentCreate.put("orderId", ((Map<String, Object>) context.get("orderItem")).get("orderId"));
        orderShipmentCreate.put("orderItemSeqId", ((Map<String, Object>) context.get("orderItem")).get("orderItemSeqId"));
        if (UtilValidate.isNotEmpty(context.get("orderItemShipGroupAssoc"))) {
            orderShipmentCreate.put("shipGroupSeqId", ((Map<String, Object>) context.get("orderItemShipGroupAssoc")).get("shipGroupSeqId"));
        }
        if (UtilValidate.isNotEmpty(context.get("orderItemShipGrpInvRes"))) {
            orderShipmentCreate.put("shipGroupSeqId", ((Map<String, Object>) context.get("orderItemShipGrpInvRes")).get("shipGroupSeqId"));
        }
        GenericValue orderShipmentLookupPk = delegator.makeValue("OrderShipment");
        orderShipmentLookupPk.setPKFields(orderShipmentCreate);
        List<GenericValue> orderShipments = null;
        try {
            orderShipments = EntityQuery.use(delegator)
                    .from("OrderShipment")
                    .where(orderShipmentLookupPk)
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderShipment: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue orderShipment = EntityUtil.getFirst((List<GenericValue>) orderShipments);
        if (UtilValidate.isEmpty(orderShipment)) {
            orderShipmentCreate.put("quantity", context.get("quantity"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createOrderShipment", orderShipmentCreate);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling createOrderShipment: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        } else {
            orderShipment.set("quantity", new BigDecimal(context.get("quantity").toString()));
            try {
                delegator.store(orderShipment);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        return result;
    }


    /**
     * Find Create ItemIssuance - meant to be called in-line
     */
    public static Map<String, Object> findCreateItemIssuance(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<GenericValue> itemIssuances = null;
        GenericValue itemIssuance = null;
        Object itemIssuanceId = null;
        Map<String, Object> itemIssuanceCreate = null;
        Map<String, Object> orderHeader = new HashMap<String, Object>();
        if (!"SALES_ORDER".equals(((Map<String, Object>) orderHeader).get("orderTypeId"))) {
            try {
                itemIssuances = EntityQuery.use(delegator)
                        .from("ItemIssuance")
                        .where(UtilMisc.toMap("orderId", ((Map<String, Object>) context.get("orderItem")).get("orderId"), "orderItemSeqId", ((Map<String, Object>) context.get("orderItem")).get("orderItemSeqId"), "shipmentId", ((Map<String, Object>) context.get("shipmentItem")).get("shipmentId"), "shipmentItemSeqId", ((Map<String, Object>) context.get("shipmentItem")).get("shipmentItemSeqId"), "shipGroupSeqId", ((Map<String, Object>) context.get("orderItemShipGroupAssoc")).get("shipGroupSeqId")))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (UtilValidate.isNotEmpty(itemIssuances)) {
                itemIssuance = EntityUtil.getFirst((List<GenericValue>) itemIssuances);
                itemIssuance.put("quantity", (BigDecimal) ((BigDecimal) itemIssuance.get("quantity$bigDecimal")).add((BigDecimal) context.get("quantity$bigDecimal")));
                try {
                    delegator.store(itemIssuance);
                } catch (Exception e) {
                    Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                itemIssuanceId = itemIssuance.get("itemIssuanceId");
                result.put("itemIssuanceId", itemIssuanceId);
                return result;
            }
        }
        itemIssuanceCreate.put("quantity", context.get("quantity"));
        itemIssuanceCreate.put("shipmentId", ((Map<String, Object>) context.get("shipmentItem")).get("shipmentId"));
        itemIssuanceCreate.put("shipmentItemSeqId", ((Map<String, Object>) context.get("shipmentItem")).get("shipmentItemSeqId"));
        itemIssuanceCreate.put("orderId", ((Map<String, Object>) context.get("orderItem")).get("orderId"));
        itemIssuanceCreate.put("orderItemSeqId", ((Map<String, Object>) context.get("orderItem")).get("orderItemSeqId"));
        itemIssuanceCreate.put("issuedDateTime", context.get("eventDate"));
        if (UtilValidate.isNotEmpty(context.get("orderItemShipGrpInvRes"))) {
            itemIssuanceCreate.put("inventoryItemId", ((Map<String, Object>) context.get("orderItemShipGrpInvRes")).get("inventoryItemId"));
            itemIssuanceCreate.put("shipGroupSeqId", ((Map<String, Object>) context.get("orderItemShipGrpInvRes")).get("shipGroupSeqId"));
        }
        if (UtilValidate.isNotEmpty(context.get("orderItemShipGroupAssoc"))) {
            itemIssuanceCreate.put("shipGroupSeqId", ((Map<String, Object>) context.get("orderItemShipGroupAssoc")).get("shipGroupSeqId"));
        }
        itemIssuanceCreate.put("issuedByUserLoginId", userLogin.get("userLoginId"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createItemIssuance", itemIssuanceCreate);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            itemIssuanceId = serviceResult.get("itemIssuanceId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createItemIssuance: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        result.put("itemIssuanceId", itemIssuanceId);

        return result;
    }


    /**
     * Associate Roles for ItemIssuance - meant to be called in-line
     */
    public static Map<String, Object> associateIssueRoles(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Object userLoginId = null;
        Map<String, Object> itemIssuanceRoleCreate = null;
        if (UtilValidate.isEmpty(userLogin.get("partyId"))) {
            userLoginId = userLogin.get("userLoginId");
            {
                String errorMsg = UtilProperties.getMessage("CommonErrorUiLabels", "CommonUserLoginNoPartyIdCannotComplete", locale);
                error_list.add(errorMsg);
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        }
        GenericValue partyRole = delegator.makeValue("PartyRole");
        partyRole.put("partyId", userLogin.get("partyId"));
        partyRole.put("roleTypeId", "PACKER");
        GenericValue checkPartyRole = null;
        try {
            checkPartyRole = EntityQuery.use(delegator)
                    .from("PartyRole")
                    .where(partyRole)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key PartyRole: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(checkPartyRole)) {
            try {
                delegator.create(partyRole);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        GenericValue itemIssuanceRole = null;
        try {
            itemIssuanceRole = EntityQuery.use(delegator)
                    .from("ItemIssuanceRole")
                    .where(UtilMisc.toMap("itemIssuanceId", context.get("itemIssuanceId"), "partyId", userLogin.get("partyId"), "roleTypeId", "PACKER"))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ItemIssuanceRole: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(itemIssuanceRole)) {
            itemIssuanceRoleCreate.put("itemIssuanceId", context.get("itemIssuanceId"));
            itemIssuanceRoleCreate.put("partyId", userLogin.get("partyId"));
            itemIssuanceRoleCreate.put("roleTypeId", "PACKER");
            itemIssuanceRoleCreate.put("shipmentId", context.get("shipmentId"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createItemIssuanceRole", itemIssuanceRoleCreate);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling createItemIssuanceRole: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        return result;
    }


    /**
     * Issue InventoryItem To FixedAssetMaint
     */
    public static Map<String, Object> issueInventoryItemToFixedAssetMaint(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue inventoryItem = null;
        try {
            inventoryItem = EntityQuery.use(delegator)
                    .from("InventoryItem")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying InventoryItem: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue fixedAssetMaint = null;
        try {
            fixedAssetMaint = EntityQuery.use(delegator)
                    .from("FixedAssetMaint")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying FixedAssetMaint: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (((Comparable) context.get("quantity")).compareTo(BigDecimal.ZERO) <= 0) {
            {
                String errorMsg = UtilProperties.getMessage("ProductUiLabels", "ProductNotIssueToFixedAssetMaintQuantityLess", locale);
                error_list.add(errorMsg);
            }
        }
        if (context.get("quantity") != null /* TODO: field compare operator greater */) {
            {
                String errorMsg = UtilProperties.getMessage("ProductUiLabels", "ProductNotIssueToFixedAssetMaintQuantityGreater", locale);
                error_list.add(errorMsg);
            }
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        Map<String, Object> itemIssuanceCreate = new HashMap<String, Object>();
        itemIssuanceCreate.put("quantity", context.get("quantity"));
        itemIssuanceCreate.put("inventoryItemId", context.get("inventoryItemId"));
        itemIssuanceCreate.put("fixedAssetId", fixedAssetMaint.get("fixedAssetId"));
        itemIssuanceCreate.put("maintHistSeqId", fixedAssetMaint.get("maintHistSeqId"));
        itemIssuanceCreate.put("issuedByUserLoginId", userLogin.get("userLoginId"));
        Object itemIssuanceId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createItemIssuance", itemIssuanceCreate);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            itemIssuanceId = serviceResult.get("itemIssuanceId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createItemIssuance: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        result.put("itemIssuanceId", itemIssuanceId);
        Map<String, Object> createDetailMap = new HashMap<String, Object>();
        createDetailMap.put("inventoryItemId", inventoryItem.get("inventoryItemId"));
        createDetailMap.put("fixedAssetId", fixedAssetMaint.get("fixedAssetId"));
        createDetailMap.put("maintHistSeqId", fixedAssetMaint.get("maintHistSeqId"));
        createDetailMap.put("itemIssuanceId", itemIssuanceId);
        ((Map<String, Object>) createDetailMap).put("quantityOnHandDiff", new BigDecimal(context.get("quantity").toString()));
        ((Map<String, Object>) createDetailMap).put("availableToPromiseDiff", new BigDecimal(context.get("quantity").toString()));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createInventoryItemDetail", createDetailMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createInventoryItemDetail: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Return the InventoryItem Issued To FixedAssetMaint
     */
    public static Map<String, Object> returnInventoryItemIssuedToFixedAssetMaint(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue itemIssuance = null;
        try {
            itemIssuance = EntityQuery.use(delegator)
                    .from("ItemIssuance")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ItemIssuance: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Object oldQuantity = itemIssuance.get("quantity");
        Map<String, Object> itemIssuanceUpdate = new HashMap<String, Object>();
        itemIssuanceUpdate.put("quantity", BigDecimal.ZERO);
        itemIssuanceUpdate.put("itemIssuanceId", context.get("itemIssuanceId"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateItemIssuance", itemIssuanceUpdate);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateItemIssuance: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> createDetailMap = new HashMap<String, Object>();
        createDetailMap.put("inventoryItemId", itemIssuance.get("inventoryItemId"));
        createDetailMap.put("fixedAssetId", itemIssuance.get("fixedAssetId"));
        createDetailMap.put("maintHistSeqId", itemIssuance.get("maintHistSeqId"));
        createDetailMap.put("itemIssuanceId", itemIssuance.get("itemIssuanceId"));
        ((Map<String, Object>) createDetailMap).put("quantityOnHandDiff", new BigDecimal(oldQuantity.toString()));
        ((Map<String, Object>) createDetailMap).put("availableToPromiseDiff", new BigDecimal(oldQuantity.toString()));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createInventoryItemDetail", createDetailMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createInventoryItemDetail: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Cancel an ItemIssuance quantity from Sales Shipment
     */
    public static Map<String, Object> cancelOrderItemIssuanceFromSalesShipment(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Object shipmentId = null;
        Map<String, Object> inlineResult = null;
        Object toCancelQuantity = null;
        Map<String, Object> reserveStoreInventoryMap = null;
        GenericValue itemIssuance = null;
        try {
            itemIssuance = EntityQuery.use(delegator)
                    .from("ItemIssuance")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ItemIssuance: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue orderHeader = null;
        try {
            orderHeader = itemIssuance.getRelatedOne("OrderHeader", false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related one OrderHeader: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue inventoryItem = null;
        try {
            inventoryItem = itemIssuance.getRelatedOne("InventoryItem", false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related one InventoryItem: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue shipment = null;
        try {
            shipment = itemIssuance.getRelatedOne("Shipment", false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related one Shipment: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (!"SHIPMENT_CANCELLED".equals(shipment.get("statusId"))) {
            shipmentId = itemIssuance.get("shipmentId");
            // TODO: Call simple-method "checkCanChangeShipmentStatusPacked" from "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml"
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        }
        if (!"SALES_ORDER".equals(orderHeader.get("orderTypeId"))) {
            {
                String errorMsg = UtilProperties.getMessage("ProductUiLabels", "ProductNotIssueOrderForNotSalesOrders", locale);
                error_list.add(errorMsg);
            }
        }
        Object qtyIssuedLeft = new BigDecimal(itemIssuance.get("cancelQuantity").toString());
        toCancelQuantity = context.get("cancelQuantity");
        if (UtilValidate.isEmpty(toCancelQuantity)) {
            toCancelQuantity = qtyIssuedLeft;
        }
        if (((Comparable) toCancelQuantity).compareTo(BigDecimal.ZERO) < 0) {
            {
                String errorMsg = UtilProperties.getMessage("ProductUiLabels", "ProductNotIssueOrderQuantityCancelLess", locale);
                error_list.add(errorMsg);
            }
        }
        if (java.util.Objects.equals(toCancelQuantity, BigDecimal.ZERO)) {
            return result;
        }
        if (toCancelQuantity != null /* TODO: field compare operator greater */) {
            {
                String errorMsg = UtilProperties.getMessage("ProductUiLabels", "ProductNotIssueOrderQuantityCancelGreater", locale);
                error_list.add(errorMsg);
            }
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        Object totalCancelQty = new BigDecimal(itemIssuance.get("cancelQuantity").toString());
        Map<String, Object> itemIssuanceUpdate = new HashMap<String, Object>();
        itemIssuanceUpdate.put("cancelQuantity", totalCancelQty);
        itemIssuanceUpdate.put("itemIssuanceId", context.get("itemIssuanceId"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateItemIssuance", itemIssuanceUpdate);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateItemIssuance: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> createDetailMap = new HashMap<String, Object>();
        createDetailMap.put("inventoryItemId", itemIssuance.get("inventoryItemId"));
        createDetailMap.put("itemIssuanceId", itemIssuance.get("itemIssuanceId"));
        createDetailMap.put("availableToPromiseDiff", toCancelQuantity);
        createDetailMap.put("quantityOnHandDiff", toCancelQuantity);
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createInventoryItemDetail", createDetailMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createInventoryItemDetail: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> reassignInventoryReservationsCtx = new HashMap<String, Object>();
        reassignInventoryReservationsCtx.put("productId", inventoryItem.get("productId"));
        reassignInventoryReservationsCtx.put("facilityId", inventoryItem.get("facilityId"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("reassignInventoryReservations", reassignInventoryReservationsCtx);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling reassignInventoryReservations: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue productStore = null;
        try {
            productStore = EntityQuery.use(delegator)
                    .from("ProductStore")
                    .where(UtilMisc.toMap("productStoreId", orderHeader.get("productStoreId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ProductStore: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if ("Y".equals(productStore.get("isImmediatelyFulfilled"))) {
            Debug.logVerbose("ProductStore with id " + productStore.get("productStoreId") + ", is immediatly fulfilled. Not reserving inventory", MODULE);
        } else {
            reserveStoreInventoryMap.put("productId", inventoryItem.get("productId"));
            reserveStoreInventoryMap.put("orderId", itemIssuance.get("orderId"));
            reserveStoreInventoryMap.put("orderItemSeqId", itemIssuance.get("orderItemSeqId"));
            reserveStoreInventoryMap.put("shipGroupSeqId", itemIssuance.get("shipGroupSeqId"));
            reserveStoreInventoryMap.put("quantity", toCancelQuantity);
            reserveStoreInventoryMap.put("productStoreId", orderHeader.get("productStoreId"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("reserveStoreInventory", reserveStoreInventoryMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling reserveStoreInventory: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        result.put("canceledQuantity", toCancelQuantity);

        return result;
    }


    /**
     * Issue InventoryItem To Shipment
     */
    public static Map<String, Object> issueInventoryItemToShipment(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Object shipmentId = context.get("shipmentId");
        Object shipmentItemSeqId = context.get("shipmentItemSeqId");
        GenericValue inventoryItem = null;
        try {
            inventoryItem = EntityQuery.use(delegator)
                    .from("InventoryItem")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying InventoryItem: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        List<GenericValue> returnItemShipments = null;
        try {
            returnItemShipments = EntityQuery.use(delegator)
                    .from("ReturnItemShipment")
                    .where(UtilMisc.toMap("shipmentId", context.get("shipmentId"), "shipmentItemSeqId", context.get("shipmentItemSeqId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue returnItemShipment = EntityUtil.getFirst((List<GenericValue>) returnItemShipments);
        Object quantityNotIssued = new BigDecimal(context.get("totalIssuedQty").toString());
        if (((Comparable) context.get("quantity")).compareTo(BigDecimal.ZERO) <= 0) {
            {
                String errorMsg = UtilProperties.getMessage("ProductUiLabels", "ProductNotIssueOrderToShipmentQuantityLess", locale);
                error_list.add(errorMsg);
            }
        }
        if (context.get("quantity") != null /* TODO: field compare operator greater */) {
            {
                String errorMsg = UtilProperties.getMessage("ProductUiLabels", "ProductNotIssueOrderToShipmentQuantityGreater", locale);
                error_list.add(errorMsg);
            }
        }
        if (context.get("quantity") != null /* TODO: field compare operator greater */) {
            {
                String errorMsg = UtilProperties.getMessage("ProductUiLabels", "ProductNotIssueOrderToShipmentQuantityReturnGreater", locale);
                error_list.add(errorMsg);
            }
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        Map<String, Object> itemIssuanceCreate = new HashMap<String, Object>();
        itemIssuanceCreate.put("quantity", context.get("quantity"));
        itemIssuanceCreate.put("inventoryItemId", context.get("inventoryItemId"));
        itemIssuanceCreate.put("shipmentId", shipmentId);
        itemIssuanceCreate.put("shipmentItemSeqId", shipmentItemSeqId);
        itemIssuanceCreate.put("issuedByUserLoginId", userLogin.get("userLoginId"));
        Object itemIssuanceId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createItemIssuance", itemIssuanceCreate);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            itemIssuanceId = serviceResult.get("itemIssuanceId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createItemIssuance: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        result.put("itemIssuanceId", itemIssuanceId);
        Map<String, Object> createDetailMap = new HashMap<String, Object>();
        createDetailMap.put("inventoryItemId", inventoryItem.get("inventoryItemId"));
        createDetailMap.put("itemIssuanceId", itemIssuanceId);
        createDetailMap.put("shipmentId", shipmentId);
        createDetailMap.put("shipmentItemSeqId", shipmentItemSeqId);
        ((Map<String, Object>) createDetailMap).put("quantityOnHandDiff", new BigDecimal(context.get("quantity").toString()));
        ((Map<String, Object>) createDetailMap).put("availableToPromiseDiff", new BigDecimal(context.get("quantity").toString()));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createInventoryItemDetail", createDetailMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createInventoryItemDetail: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Computes the total quantity assigned to shipment for a purchase order item
     */
    public static Map<String, Object> getTotalIssuedQuantityForOrderItem(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        BigDecimal totalIssuedQuantity = null;
        List<GenericValue> allItemIssuances = null;
        totalIssuedQuantity = BigDecimal.ZERO;
        List<GenericValue> orderShipments = null;
        try {
            orderShipments = EntityQuery.use(delegator)
                    .from("OrderShipment")
                    .where(UtilMisc.toMap("orderId", ((Map<String, Object>) context.get("orderItem")).get("orderId"), "orderItemSeqId", ((Map<String, Object>) context.get("orderItem")).get("orderItemSeqId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue orderShipment = null;
        GenericValue itemIssuance = null;
        if (UtilValidate.isNotEmpty(orderShipments)) {
            if (orderShipments != null) {
                for (GenericValue orderShipmentEntry : orderShipments) {
                    totalIssuedQuantity = (BigDecimal) ((BigDecimal) context.get("totalIssuedQuantity$bigDecimal")).add((BigDecimal) orderShipmentEntry.get("quantity$bigDecimal"));
                }
            }
        } else {
            try {
                allItemIssuances = EntityQuery.use(delegator)
                        .from("ItemIssuance")
                        .where(UtilMisc.toMap("orderId", ((Map<String, Object>) context.get("orderItem")).get("orderId"), "orderItemSeqId", ((Map<String, Object>) context.get("orderItem")).get("orderItemSeqId")))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (allItemIssuances != null) {
                for (GenericValue itemIssuanceEntry : allItemIssuances) {
                    totalIssuedQuantity = (BigDecimal) ((BigDecimal) context.get("totalIssuedQuantity$bigDecimal")).add((BigDecimal) itemIssuanceEntry.get("quantity$bigDecimal"));
                }
            }
        }

        return result;
    }

}

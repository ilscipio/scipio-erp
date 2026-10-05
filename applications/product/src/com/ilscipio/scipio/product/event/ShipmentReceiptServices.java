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
import java.math.RoundingMode;
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
 * <p>Generated from: component://product/script/org/ofbiz/shipment/receipt/ShipmentReceiptServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class ShipmentReceiptServices {

    private static final String MODULE = ShipmentReceiptServices.class.getName();


    /**
     * Create a ShipmentReceipt
     */
    public static Map<String, Object> createShipmentReceipt(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue newEntity = null;
        Timestamp nowTimestamp = null;
        GenericValue invDet = null;
        Boolean affectAccounting = null;
        newEntity = delegator.makeValue("ShipmentReceipt");
        newEntity.setNonPKFields(context);
        String receiptId = delegator.getNextSeqId("ShipmentReceipt");
        receiptId = receiptId != null ? receiptId.toString() : null;
        newEntity.put("receiptId", receiptId);
        result.put("receiptId", receiptId);
        if (UtilValidate.isEmpty(newEntity.get("datetimeReceived"))) {
            nowTimestamp = new Timestamp(System.currentTimeMillis());
            newEntity.put("datetimeReceived", nowTimestamp);
        }
        newEntity.put("receivedByUserLoginId", userLogin.get("userLoginId"));
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(context.get("inventoryItemDetailSeqId"))) {
            try {
                invDet = EntityQuery.use(delegator)
                        .from("InventoryItemDetail")
                        .where(UtilMisc.toMap("inventoryItemDetailSeqId", context.get("inventoryItemDetailSeqId"), "inventoryItemId", context.get("inventoryItemId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying InventoryItemDetail: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            invDet.put("receiptId", receiptId);
            try {
                delegator.store(invDet);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        affectAccounting = Boolean.TRUE;
        GenericValue product = null;
        try {
            product = EntityQuery.use(delegator)
                    .from("Product")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Product: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (("SERVICE_PRODUCT".equals(product.get("productTypeId")) || "ASSET_USAGE_OUT_IN".equals(product.get("productTypeId")) || "AGGREGATEDSERV_CONF".equals(product.get("productTypeId")))) {
            affectAccounting = Boolean.FALSE;
        }
        result.put("affectAccounting", affectAccounting);

        return result;
    }


    /**
     * Create a ShipmentReceipt Role
     */
    public static Map<String, Object> createShipmentReceiptRole(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue newEntity = delegator.makeValue("ShipmentReceiptRole");
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
     * Remove a ShipmentReceipt Role
     */
    public static Map<String, Object> removeShipmentReceiptRole(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookupPKMap = delegator.makeValue("ShipmentReceiptRole");
        lookupPKMap.setPKFields(context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ShipmentReceiptRole")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ShipmentReceiptRole: " + e.getMessage(), MODULE);
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
     * Receive Inventory in new Inventory Item(s)
     */
    public static Map<String, Object> receiveInventoryProduct(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Object loops = null;
        List<GenericValue> orderRoles = null;
        GenericValue orderRole = null;
        Object currentInventoryItemId = null;
        Map<String, Object> serviceInMap = null;
        GenericValue inventoryItem = null;
        loops = 1D;
        if ("SERIALIZED_INV_ITEM".equals(context.get("inventoryItemTypeId"))) {
            if (((!(UtilValidate.isEmpty(context.get("serialNumber"))) || !(UtilValidate.isEmpty(context.get("currentInventoryItemId")))) && ((Comparable) context.get("quantityAccepted")).compareTo(BigDecimal.ONE) > 0)) {
                {
                    String errorMsg = UtilProperties.getMessage("ProductUiLabels", "FacilityReceiveInventoryProduct", locale);
                    error_list.add(errorMsg);
                }
            }
            loops = context.get("quantityAccepted");
            context.put("quantityAccepted", BigDecimal.ONE);
        }
        context.put("quantityOnHandDiff", context.get("quantityAccepted"));
        context.put("availableToPromiseDiff", context.get("quantityAccepted"));
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        if ("NON_SERIAL_INV_ITEM".equals(context.get("inventoryItemTypeId"))) {
            if ("INV_DEFECTIVE".equals(context.get("statusId"))) {
                context.put("statusId", "INV_NS_DEFECTIVE");
            } else {
                if ("INV_ON_HOLD".equals(context.get("statusId"))) {
                    context.put("statusId", "INV_NS_ON_HOLD");
                } else {
                    if ("INV_RETURNED".equals(context.get("statusId"))) {
                        context.put("statusId", "INV_NS_RETURNED");
                    }
                }
            }
            Object parameters_statusId = null;
            if ((!("INV_NS_DEFECTIVE".equals(context.get("statusId"))) && !("INV_NS_ON_HOLD".equals(context.get("statusId"))) && !("INV_NS_RETURNED".equals(context.get("statusId"))))) {
                context.put("statusId", null);
            }
        }
        for (int currentLoop = 0; currentLoop < ((Number) loops).intValue(); currentLoop++) {
            Debug.logInfo("receiveInventoryProduct Looping and creating inventory info - " + context.get("currentLoop"), MODULE);
            serviceInMap = new HashMap<String, Object>();
            currentInventoryItemId = null;
            if (UtilValidate.isNotEmpty(context.get("orderId"))) {
                try {
                    orderRoles = EntityQuery.use(delegator)
                            .from("OrderRole")
                            .where(UtilMisc.toMap("orderId", context.get("orderId"), "roleTypeId", "SUPPLIER_AGENT"))
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (UtilValidate.isNotEmpty(orderRoles)) {
                    orderRole = EntityUtil.getFirst((List<GenericValue>) orderRoles);
                    context.put("partyId", orderRole.get("partyId"));
                }
            }
            if (UtilValidate.isEmpty(context.get("currentInventoryItemId"))) {
                // set-service-fields from "parameters" to "serviceInMap" for service "createInventoryItem"
                serviceInMap.putAll(UtilMisc.toMap(context));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createInventoryItem", serviceInMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                    currentInventoryItemId = serviceResult.get("inventoryItemId");
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createInventoryItem: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            } else {
                if (UtilValidate.isNotEmpty(context.get("currentInventoryItemId"))) {
                    context.put("inventoryItemId", context.get("currentInventoryItemId"));
                }
                // set-service-fields from "parameters" to "serviceInMap" for service "updateInventoryItem"
                serviceInMap.putAll(UtilMisc.toMap(context));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("updateInventoryItem", serviceInMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling updateInventoryItem: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                currentInventoryItemId = context.get("currentInventoryItemId");
            }
            if (!"SERIALIZED_INV_ITEM".equals(context.get("inventoryItemTypeId"))) {
                serviceInMap = new HashMap<String, Object>();
                // set-service-fields from "parameters" to "serviceInMap" for service "createInventoryItemDetail"
                serviceInMap.putAll(UtilMisc.toMap(context));
                serviceInMap.put("inventoryItemId", currentInventoryItemId);
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createInventoryItemDetail", serviceInMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                    context.put("inventoryItemDetailSeqId", serviceResult.get("inventoryItemDetailSeqId"));
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createInventoryItemDetail: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
            serviceInMap = new HashMap<String, Object>();
            // set-service-fields from "parameters" to "serviceInMap" for service "createShipmentReceipt"
            serviceInMap.putAll(UtilMisc.toMap(context));
            serviceInMap.put("inventoryItemId", currentInventoryItemId);
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createShipmentReceipt", serviceInMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling createShipmentReceipt: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            Object serviceInMap_inventoryItemId = null;
            Object serviceInMap_statusId = null;
            if (("SERIALIZED_INV_ITEM".equals(context.get("inventoryItemTypeId")) && UtilValidate.isEmpty(context.get("returnId")))) {
                try {
                    inventoryItem = EntityQuery.use(delegator)
                            .from("InventoryItem")
                            .where(UtilMisc.toMap("inventoryItemId", currentInventoryItemId))
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying InventoryItem: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if ((!"INV_PROMISED".equals(inventoryItem.get("statusId")) && !"INV_ON_HOLD".equals(inventoryItem.get("statusId")))) {
                    serviceInMap = new HashMap<String, Object>();
                    serviceInMap.put("inventoryItemId", currentInventoryItemId);
                    serviceInMap.put("statusId", "INV_AVAILABLE");
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("updateInventoryItem", serviceInMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling updateInventoryItem: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            }
            serviceInMap = new HashMap<String, Object>();
            // set-service-fields from "parameters" to "serviceInMap" for service "balanceInventoryItems"
            serviceInMap.putAll(UtilMisc.toMap(context));
            serviceInMap.put("inventoryItemId", currentInventoryItemId);
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("balanceInventoryItems", serviceInMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling balanceInventoryItems: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            List<Object> successMessageList = new LinkedList<>();
            successMessageList.add("Received " + context.get("quantityAccepted") + " of " + context.get("productId") + " in inventory item " + currentInventoryItemId);
        }
        result.put("inventoryItemId", currentInventoryItemId);

        return result;
    }


    /**
     * Quick Receive Entire Return
     */
    public static Map<String, Object> quickReceiveReturn(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Object setNonSerial = null;
        List<GenericValue> returnItems = null;
        Long serializedItemCount = null;
        GenericValue orderItem = null;
        Map<String, Object> shipmentCtx = new HashMap<>();
        Map<String, Object> costCtx = new HashMap<>();
        Object shipmentItemSeqId = null;
        GenericValue returnItem = null;
        GenericValue returnHeader = null;
        Long iiCount = null;
        Map<String, Object> retStCtx = new HashMap<>();
        Object nonProductItems = null;
        Object shipmentId = null;
        Object shipItemCtx = null;
        Object receiveCtx = null;
        GenericValue facility = null;
        Timestamp nowTimestamp = null;
        Long returnItemCount = null;
        try {
            returnHeader = EntityQuery.use(delegator)
                    .from("ReturnHeader")
                    .where(UtilMisc.toMap("returnId", context.get("returnId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ReturnHeader: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if ("Y".equals(returnHeader.get("needsInventoryReceive"))) {
            iiCount = null;
            try {
                iiCount = EntityQuery.use(delegator)
                        .from("InventoryItem")
                        .where("facilityId", returnHeader.get("destinationFacilityId"))
                        .queryCount();
            } catch (Exception e) {
                Debug.logError(e, "Error counting InventoryItem: " + e.getMessage(), MODULE);
            }
            if (((Comparable) iiCount).compareTo(0) > 0) {
                shipmentCtx.put("returnId", context.get("returnId"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createShipmentForReturn", shipmentCtx);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                    shipmentId = serviceResult.get("shipmentId");
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createShipmentForReturn: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                Debug.logInfo("Created new shipment " + shipmentId, MODULE);
                try {
                    returnItems = EntityQuery.use(delegator)
                            .from("ReturnItem")
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying ReturnItem: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (UtilValidate.isEmpty(context.get("inventoryItemTypeId"))) {
                    try {
                        facility = returnHeader.getRelatedOne("Facility", false);
                    } catch (Exception e) {
                        Debug.logError(e, "Error getting related one Facility: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    context.put("inventoryItemTypeId", facility.get("defaultInventoryItemTypeId"));
                }
                nowTimestamp = new Timestamp(System.currentTimeMillis());
                returnItemCount = null;
                try {
                    returnItemCount = EntityQuery.use(delegator)
                            .from("ReturnItem")
                            .where("returnId", returnHeader.get("returnId"))
                            .queryCount();
                } catch (Exception e) {
                    Debug.logError(e, "Error counting ReturnItem: " + e.getMessage(), MODULE);
                }
                nonProductItems = 0L;
                if (returnItems != null) {
                    for (GenericValue returnItem_iter : returnItems) {
                        returnItem = returnItem_iter;
                        shipItemCtx = new HashMap<String, Object>();
                        ((Map<String, Object>) shipItemCtx).put("shipmentId", shipmentId);
                        ((Map<String, Object>) shipItemCtx).put("productId", returnItem.get("productId"));
                        ((Map<String, Object>) shipItemCtx).put("quantity", returnItem.get("returnQuantity"));
                        Debug.logInfo("calling create shipment item with " + shipItemCtx, MODULE);
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("createShipmentItem", (Map<String, Object>) shipItemCtx);
                            if (ServiceUtil.isError(serviceResult)) {
                                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                            }
                            shipmentItemSeqId = serviceResult.get("shipmentItemSeqId");
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling createShipmentItem: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                    }
                }
                if (returnItems != null) {
                    for (GenericValue returnItem_iter : returnItems) {
                        returnItem = returnItem_iter;
                        receiveCtx = null;
                        if (UtilValidate.isEmpty(returnItem.get("expectedItemStatus"))) {
                            returnItem.put("expectedItemStatus", "INV_RETURNED");
                        }
                        try {
                            orderItem = returnItem.getRelatedOne("OrderItem", false);
                        } catch (Exception e) {
                            Debug.logError(e, "Error getting related one OrderItem: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                        if (UtilValidate.isNotEmpty(orderItem.get("productId"))) {
                            costCtx.put("returnItemSeqId", returnItem.get("returnItemSeqId"));
                            costCtx.put("returnId", returnItem.get("returnId"));
                            try {
                                Map<String, Object> serviceResult = dispatcher.runSync("getReturnItemInitialCost", costCtx);
                                if (ServiceUtil.isError(serviceResult)) {
                                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                                }
                                ((Map<String, Object>) receiveCtx).put("unitCost", serviceResult.get("initialItemCost"));
                            } catch (Exception e) {
                                Debug.logError(e, "Error calling getReturnItemInitialCost: " + e.getMessage(), MODULE);
                                return ServiceUtil.returnError(e.getMessage());
                            }
                            serializedItemCount = null;
                            try {
                                serializedItemCount = EntityQuery.use(delegator)
                                        .from("InventoryItem")
                                        .queryCount();
                            } catch (Exception e) {
                                Debug.logError(e, "Error counting InventoryItem: " + e.getMessage(), MODULE);
                            }
                            setNonSerial = "false";
                            if ("NON_SERIAL_INV_ITEM".equals(context.get("inventoryItemTypeId"))) {
                                if ("0".equals(serializedItemCount)) {
                                    context.put("inventoryItemTypeId", "NON_SERIAL_INV_ITEM");
                                    setNonSerial = "true";
                                }
                            }
                            if ("false".equals(setNonSerial)) {
                                context.put("inventoryItemTypeId", "SERIALIZED_INV_ITEM");
                                returnItem.put("returnQuantity", BigDecimal.ONE);
                            }
                            ((Map<String, Object>) receiveCtx).put("inventoryItemTypeId", context.get("inventoryItemTypeId"));
                            ((Map<String, Object>) receiveCtx).put("statusId", returnItem.get("expectedItemStatus"));
                            ((Map<String, Object>) receiveCtx).put("productId", returnItem.get("productId"));
                            ((Map<String, Object>) receiveCtx).put("returnItemSeqId", returnItem.get("returnItemSeqId"));
                            ((Map<String, Object>) receiveCtx).put("returnId", returnItem.get("returnId"));
                            ((Map<String, Object>) receiveCtx).put("quantityAccepted", returnItem.get("returnQuantity"));
                            ((Map<String, Object>) receiveCtx).put("facilityId", returnHeader.get("destinationFacilityId"));
                            ((Map<String, Object>) receiveCtx).put("shipmentId", shipmentId);
                            ((Map<String, Object>) receiveCtx).put("comments", "Returned Item RA# " + returnItem.get("returnId"));
                            ((Map<String, Object>) receiveCtx).put("datetimeReceived", nowTimestamp);
                            ((Map<String, Object>) receiveCtx).put("quantityRejected", BigDecimal.ZERO);
                            try {
                                Map<String, Object> serviceResult = dispatcher.runSync("receiveInventoryProduct", (Map<String, Object>) receiveCtx);
                                if (ServiceUtil.isError(serviceResult)) {
                                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                                }
                            } catch (Exception e) {
                                Debug.logError(e, "Error calling receiveInventoryProduct: " + e.getMessage(), MODULE);
                                return ServiceUtil.returnError(e.getMessage());
                            }
                        } else {
                            nonProductItems = (BigDecimal.ZERO).longValue();
                        }
                    }
                }
                try {
                    returnHeader.refresh();
                } catch (Exception e) {
                    Debug.logError(e, "Error refreshing value: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                returnHeader.put("needsInventoryReceive", "N");
                try {
                    delegator.store(returnHeader);
                } catch (Exception e) {
                    Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (!"RETURN_RECEIVED".equals(returnHeader.get("statusId"))) {
                    retStCtx.put("returnId", returnHeader.get("returnId"));
                    retStCtx.put("statusId", "RETURN_RECEIVED");
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("updateReturnHeader", retStCtx);
                        if (ServiceUtil.isError(serviceResult)) {
                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling updateReturnHeader: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            } else {
                Debug.logInfo("Not receiving inventory for returnId " + returnHeader.get("returnId") + ", no inventory information available.", MODULE);
            }
        }

        return result;
    }


    /**
     * Issues order item quantity specified to the shipment, then receives inventory for that item and quantity
     */
    public static Map<String, Object> issueOrderItemToShipmentAndReceiveAgainstPO(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue shipmentItem = null;
        List<GenericValue> shipmentItems = null;
        GenericValue orderShipment = null;
        Map<String, Object> orderShipmentCreate = null;
        BigDecimal quantityToAdd = null;
        Map<String, Object> shipmentItemLookupPk = null;
        Map<String, Object> shipmentItemCreate = null;
        List<GenericValue> orderShipments = null;
        BigDecimal receivedQuantity = null;
        Object shipmentItemSeqId = null;
        Map<String, Object> inlineResult = null;
        GenericValue shipmentReceipt = null;
        List<GenericValue> shipmentReceipts = null;
        Object operationName = "Issue OrderItem to Shipment and Receive against PO";
        // TODO: Call simple-method "checkCanChangeShipmentStatusPacked" from "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml"
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
        if (UtilValidate.isNotEmpty(orderItem.get("productId"))) {
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
            shipmentItemCreate.put("productId", orderItem.get("productId"));
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
            orderShipmentCreate.put("quantity", context.get("quantity"));
            orderShipmentCreate.put("shipmentId", shipmentItem.get("shipmentId"));
            orderShipmentCreate.put("shipmentItemSeqId", shipmentItem.get("shipmentItemSeqId"));
            orderShipmentCreate.put("orderId", orderItem.get("orderId"));
            orderShipmentCreate.put("orderItemSeqId", orderItem.get("orderItemSeqId"));
            if (UtilValidate.isNotEmpty(orderItemShipGroupAssoc)) {
                orderShipmentCreate.put("shipGroupSeqId", orderItemShipGroupAssoc.get("shipGroupSeqId"));
            }
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
            // TODO: Call simple-method "getTotalIssuedQuantityForOrderItem" from "component://product/script/org/ofbiz/shipment/issuance/IssuanceServices.xml"
            inlineResult = getReceivedQuantityForOrderItem(dctx, context);
            if (ServiceUtil.isError(inlineResult)) {
                return inlineResult;
            }
            receivedQuantity = (BigDecimal) ((BigDecimal) context.get("receivedQuantity$bigDecimal")).add((BigDecimal) context.get("quantity$bigDecimal"));
            try {
                orderShipments = EntityQuery.use(delegator)
                        .from("OrderShipment")
                        .where(UtilMisc.toMap("orderId", orderItem.get("orderId"), "orderItemSeqId", orderItem.get("orderItemSeqId"), "shipmentId", shipmentItem.get("shipmentId"), "shipmentItemSeqId", shipmentItem.get("shipmentItemSeqId"), "shipGroupSeqId", orderItemShipGroupAssoc.get("shipGroupSeqId")))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            orderShipment = EntityUtil.getFirst((List<GenericValue>) orderShipments);
            if (context.get("totalIssuedQuantity") != null /* TODO: field compare operator less */) {
                quantityToAdd = (BigDecimal) ((BigDecimal) context.get("receivedQuantity$bigDecimal")).subtract((BigDecimal) context.get("totalIssuedQuantity$bigDecimal"));
                shipmentItem.put("quantity", (BigDecimal) ((BigDecimal) shipmentItem.get("quantity$bigDecimal")).add((BigDecimal) context.get("quantityToAdd$bigDecimal")));
                try {
                    delegator.store(shipmentItem);
                } catch (Exception e) {
                    Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                shipmentItemSeqId = shipmentItem.get("shipmentItemSeqId");
                orderShipment.put("quantity", (BigDecimal) ((BigDecimal) orderShipment.get("quantity$bigDecimal")).add((BigDecimal) context.get("quantityToAdd$bigDecimal")));
                try {
                    delegator.store(orderShipment);
                } catch (Exception e) {
                    Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }
        Map<String, Object> receiveInventoryProductCtx = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "receiveInventoryProductCtx" for service "receiveInventoryProduct"
        receiveInventoryProductCtx.putAll(UtilMisc.toMap(context));
        receiveInventoryProductCtx.put("shipmentItemSeqId", shipmentItemSeqId);
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("receiveInventoryProduct", receiveInventoryProductCtx);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            result.put("inventoryItemId", serviceResult.get("inventoryItemId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling receiveInventoryProduct: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Computes the till now received quantity from all ShipmentReceipts
     */
    public static Map<String, Object> getReceivedQuantityForOrderItem(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        BigDecimal receivedQuantity = null;
        receivedQuantity = BigDecimal.ZERO;
        List<GenericValue> shipmentReceipts = null;
        try {
            shipmentReceipts = EntityQuery.use(delegator)
                    .from("ShipmentReceipt")
                    .where(UtilMisc.toMap("orderId", ((Map<String, Object>) context.get("orderItem")).get("orderId"), "orderItemSeqId", ((Map<String, Object>) context.get("orderItem")).get("orderItemSeqId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (shipmentReceipts != null) {
            for (GenericValue shipmentReceipt : shipmentReceipts) {
                receivedQuantity = (BigDecimal) ((BigDecimal) context.get("receivedQuantity$bigDecimal")).add((BigDecimal) shipmentReceipt.get("quantityAccepted$bigDecimal"));
            }
        }

        return result;
    }


    /**
     * Update issuance, shipment and order items if quantity received is higher than quantity on purchase order
     */
    public static Map<String, Object> updateIssuanceShipmentAndPoOnReceiveInventory(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue orderItem = null;
        BigDecimal quantityVariance = null;
        List<GenericValue> orderItemShipGroupAssocs = null;
        BigDecimal oisgaQuantity = null;
        GenericValue orderItemShipGroupAssoc = null;
        GenericValue orderShipment = null;
        BigDecimal quantityToAdd = null;
        List<GenericValue> orderShipments = null;
        GenericValue shipmentItem = null;
        List<GenericValue> shipmentItems = null;
        Map<String, Object> inlineResult = null;
        BigDecimal receivedQuantity = null;
        GenericValue shipmentReceipt = null;
        List<GenericValue> shipmentReceipts = null;
        try {
            orderItem = EntityQuery.use(delegator)
                    .from("OrderItem")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderItem: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(context.get("orderCurrencyUnitPrice"))) {
            if (!java.util.Objects.equals(context.get("orderCurrencyUnitPrice"), orderItem.get("unitPrice"))) {
                orderItem.put("unitPrice", context.get("orderCurrencyUnitPrice"));
                try {
                    delegator.store(orderItem);
                } catch (Exception e) {
                    Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        } else {
            if (!java.util.Objects.equals(context.get("unitCost"), orderItem.get("unitPrice"))) {
                orderItem.put("unitPrice", context.get("unitCost"));
                try {
                    delegator.store(orderItem);
                } catch (Exception e) {
                    Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }
        inlineResult = getReceivedQuantityForOrderItem(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (orderItem.get("quantity") != null /* TODO: field compare operator less */) {
            try {
                orderItemShipGroupAssocs = EntityQuery.use(delegator)
                        .from("OrderItemShipGroupAssoc")
                        .where(UtilMisc.toMap("orderId", orderItem.get("orderId"), "orderItemSeqId", orderItem.get("orderItemSeqId")))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            quantityVariance = ((new BigDecimal(receivedQuantity.toString())).subtract(new BigDecimal(orderItem.get("quantity").toString()))).setScale(2, RoundingMode.HALF_UP);
            orderItemShipGroupAssoc = EntityUtil.getFirst((List<GenericValue>) orderItemShipGroupAssocs);
            oisgaQuantity = ((new BigDecimal(orderItemShipGroupAssoc.get("quantity").toString())).add(new BigDecimal(quantityVariance.toString()))).setScale(2, RoundingMode.HALF_UP);
            orderItemShipGroupAssoc.put("quantity", oisgaQuantity);
            orderItem.put("quantity", receivedQuantity);
            try {
                delegator.store(orderItemShipGroupAssoc);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            try {
                delegator.store(orderItem);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        if (UtilValidate.isNotEmpty(context.get("shipmentId"))) {
            if (UtilValidate.isNotEmpty(orderItem.get("productId"))) {
                // TODO: Call simple-method "getTotalIssuedQuantityForOrderItem" from "component://product/script/org/ofbiz/shipment/issuance/IssuanceServices.xml"
                if (context.get("totalIssuedQuantity") != null /* TODO: field compare operator less */) {
                    quantityToAdd = (BigDecimal) ((BigDecimal) context.get("receivedQuantity$bigDecimal")).subtract((BigDecimal) context.get("totalIssuedQuantity$bigDecimal"));
                    try {
                        shipmentItems = EntityQuery.use(delegator)
                                .from("ShipmentItem")
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying ShipmentItem: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    shipmentItem = EntityUtil.getFirst((List<GenericValue>) shipmentItems);
                    shipmentItem.put("quantity", (BigDecimal) ((BigDecimal) shipmentItem.get("quantity$bigDecimal")).add((BigDecimal) context.get("quantityToAdd$bigDecimal")));
                    try {
                        delegator.store(shipmentItem);
                    } catch (Exception e) {
                        Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    try {
                        orderShipments = EntityQuery.use(delegator)
                                .from("OrderShipment")
                                .where(UtilMisc.toMap("orderId", context.get("orderId"), "orderItemSeqId", context.get("orderItemSeqId"), "shipmentId", context.get("shipmentId"), "shipmentItemSeqId", shipmentItem.get("shipmentItemSeqId")))
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    orderShipment = EntityUtil.getFirst((List<GenericValue>) orderShipments);
                    orderShipment.put("quantity", (BigDecimal) ((BigDecimal) orderShipment.get("quantity$bigDecimal")).add((BigDecimal) context.get("quantityToAdd$bigDecimal")));
                    try {
                        delegator.store(orderShipment);
                    } catch (Exception e) {
                        Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            }
        }

        return result;
    }


    /**
     * Cancel Received Items against a purchase order if received something incorrectly
     */
    public static Map<String, Object> cancelReceivedItems(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> shipmentStatusMap = null;
        List<GenericValue> orderItemBillings = null;
        GenericValue orderItem = null;
        GenericValue orderItemBilling = null;
        Map<String, Object> invoiceStatusMap = null;
        Map<String, Object> orderItemCtx = new HashMap<>();
        GenericValue orderHeader = null;
        GenericValue shipmentReceipt = null;
        try {
            shipmentReceipt = EntityQuery.use(delegator)
                    .from("ShipmentReceipt")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ShipmentReceipt: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        shipmentReceipt.put("quantityAccepted", BigDecimal.ZERO);
        shipmentReceipt.put("quantityRejected", BigDecimal.ZERO);
        try {
            delegator.store(shipmentReceipt);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue inventoryItem = null;
        try {
            inventoryItem = shipmentReceipt.getRelatedOne("InventoryItem", false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related one InventoryItem: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> inventoryItemDetailMap = new HashMap<String, Object>();
        inventoryItemDetailMap.put("inventoryItemId", inventoryItem.get("inventoryItemId"));
        ((Map<String, Object>) inventoryItemDetailMap).put("quantityOnHandDiff", new BigDecimal(inventoryItem.get("quantityOnHandTotal").toString()));
        ((Map<String, Object>) inventoryItemDetailMap).put("availableToPromiseDiff", new BigDecimal(inventoryItem.get("availableToPromiseTotal").toString()));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createInventoryItemDetail", inventoryItemDetailMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createInventoryItemDetail: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> balanceInventoryItemMap = new HashMap<String, Object>();
        balanceInventoryItemMap.put("inventoryItemId", inventoryItem.get("inventoryItemId"));
        balanceInventoryItemMap.put("priorityOrderId", shipmentReceipt.get("orderId"));
        balanceInventoryItemMap.put("priorityOrderItemSeqId", shipmentReceipt.get("orderItemSeqId"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("balanceInventoryItems", balanceInventoryItemMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling balanceInventoryItems: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue shipment = null;
        try {
            shipment = shipmentReceipt.getRelatedOne("Shipment", false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related one Shipment: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if ("PURCH_SHIP_RECEIVED".equals(shipment.get("statusId"))) {
            shipmentStatusMap.put("shipmentId", shipment.get("shipmentId"));
            shipmentStatusMap.put("statusId", "PURCH_SHIP_SHIPPED");
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updateShipment", shipmentStatusMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling updateShipment: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        try {
            orderItem = shipmentReceipt.getRelatedOne("OrderItem", false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related one OrderItem: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if ("ITEM_COMPLETED".equals(orderItem.get("statusId"))) {
            orderItem.put("statusId", "ITEM_APPROVED");
            // set-service-fields from "orderItem" to "orderItemCtx" for service "changeOrderItemStatus"
            orderItemCtx.putAll(UtilMisc.toMap(orderItem));
            orderItemCtx.put("fromStatusId", "ITEM_COMPLETED");
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("changeOrderItemStatus", orderItemCtx);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling changeOrderItemStatus: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            try {
                orderHeader = orderItem.getRelatedOne("OrderHeader", false);
            } catch (Exception e) {
                Debug.logError(e, "Error getting related one OrderHeader: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            try {
                orderItemBillings = EntityQuery.use(delegator)
                        .from("OrderItemBilling")
                        .where(UtilMisc.toMap("orderId", orderItem.get("orderId")))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (UtilValidate.isNotEmpty(orderItemBillings)) {
                orderItemBilling = EntityUtil.getFirst((List<GenericValue>) orderItemBillings);
                invoiceStatusMap.put("invoiceId", orderItemBilling.get("invoiceId"));
                invoiceStatusMap.put("statusId", "INVOICE_CANCELLED");
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("setInvoiceStatus", invoiceStatusMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling setInvoiceStatus: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }

        return result;
    }

}

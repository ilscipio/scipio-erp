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
import org.ofbiz.base.util.GroovyUtil;
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
 * <p>Generated from: component://product/script/org/ofbiz/product/inventory/InventoryReserveServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class InventoryReserveServices {

    private static final String MODULE = InventoryReserveServices.class.getName();


    /**
     * Reserve Inventory for a Product
     */
    public static Map<String, Object> reserveProductInventory(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue newNonSerInventoryItem = null;
        GenericValue permUserLogin = null;
        List<GenericValue> inventoryItemAndLocations = null;
        GenericValue orderHeader = null;
        GenericValue inventoryItemAndLocation = null;
        GenericValue productFacility = null;
        Map<String, Object> inlineResult = null;
        List<GenericValue> inventoryItems = null;
        Object createInventoryItemOutMap = null;
        Map<String, Object> reserveOisgirMap = null;
        Object createInventoryItemInMap = null;
        Object orderByString = null;
        Long daysToShip = null;
        Map<String, Object> createDetailMap = null;
        GenericValue inventoryItem = null;
        Object ebayReserveReasonEnumId = null;
        GenericValue lastNonSerInventoryItem = null;
        Debug.logVerbose("Parameters : " + context, MODULE);
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
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
        GenericValue facility = null;
        try {
            facility = EntityQuery.use(delegator)
                    .from("Facility")
                    .where(context)
                    .cache()
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Facility: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue productType = null;
        try {
            productType = product.getRelatedOne("ProductType", false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related one ProductType: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if ("N".equals(productType.get("isPhysical"))) {
            context.put("quantityNotReserved", BigDecimal.ZERO);
        } else {
            try {
                orderHeader = EntityQuery.use(delegator)
                        .from("OrderHeader")
                        .where(context)
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying OrderHeader: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if ("INVRO_GUNIT_COST".equals(context.get("reserveOrderEnumId"))) {
                orderByString = "-unitCost";
            } else {
                if ("INVRO_LUNIT_COST".equals(context.get("reserveOrderEnumId"))) {
                    orderByString = "+unitCost";
                } else {
                    if ("INVRO_FIFO_EXP".equals(context.get("reserveOrderEnumId"))) {
                        orderByString = "+expireDate";
                    } else {
                        if ("INVRO_LIFO_EXP".equals(context.get("reserveOrderEnumId"))) {
                            orderByString = "-expireDate";
                        } else {
                            if ("INVRO_LIFO_REC".equals(context.get("reserveOrderEnumId"))) {
                                orderByString = "-datetimeReceived";
                            } else {
                                orderByString = "+datetimeReceived";
                                context.put("reserveOrderEnumId", "INVRO_FIFO_REC");
                            }
                        }
                    }
                }
            }
            context.put("quantityNotReserved", context.get("quantity"));
            try {
                inventoryItemAndLocations = EntityQuery.use(delegator)
                        .from("InventoryItemAndLocation")
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying InventoryItemAndLocation: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (inventoryItemAndLocations != null) {
                for (GenericValue inventoryItemAndLocation_iter : inventoryItemAndLocations) {
                    inventoryItemAndLocation = inventoryItemAndLocation_iter;
                    if (((Comparable) context.get("quantityNotReserved")).compareTo(0D) > 0) {
                        inventoryItem = delegator.makeValue("InventoryItem");
                        context.put("inventoryItem", inventoryItem);
                        context.put("reserveOisgirMap", reserveOisgirMap);
                        context.put("ebayReserveReasonEnumId", ebayReserveReasonEnumId);
                        context.put("createDetailMap", createDetailMap);
                        context.put("lastNonSerInventoryItem", lastNonSerInventoryItem);
                        reserveForInventoryItemInline(dctx, context);
                        context = (Map<String, Object>) context.get("context");
                        inlineResult = (Map<String, Object>) context.get("inlineResult");
                    }
                }
            }
            if (((Comparable) context.get("quantityNotReserved")).compareTo(BigDecimal.ZERO) > 0) {
                try {
                    inventoryItemAndLocations = EntityQuery.use(delegator)
                            .from("InventoryItemAndLocation")
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying InventoryItemAndLocation: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (inventoryItemAndLocations != null) {
                    for (GenericValue inventoryItemAndLocation_iter : inventoryItemAndLocations) {
                        inventoryItemAndLocation = inventoryItemAndLocation_iter;
                        if (((Comparable) context.get("quantityNotReserved")).compareTo(0D) > 0) {
                            inventoryItem = delegator.makeValue("InventoryItem");
                            context.put("inventoryItem", inventoryItem);
                            context.put("reserveOisgirMap", reserveOisgirMap);
                            context.put("ebayReserveReasonEnumId", ebayReserveReasonEnumId);
                            context.put("createDetailMap", createDetailMap);
                            context.put("lastNonSerInventoryItem", lastNonSerInventoryItem);
                            reserveForInventoryItemInline(dctx, context);
                            context = (Map<String, Object>) context.get("context");
                            inlineResult = (Map<String, Object>) context.get("inlineResult");
                        }
                    }
                }
            }
            if (((Comparable) context.get("quantityNotReserved")).compareTo(BigDecimal.ZERO) > 0) {
                try {
                    inventoryItems = EntityQuery.use(delegator)
                            .from("InventoryItem")
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying InventoryItem: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (inventoryItems != null) {
                    for (GenericValue inventoryItem_iter : inventoryItems) {
                        inventoryItem = inventoryItem_iter;
                        if ((((Comparable) context.get("quantityNotReserved")).compareTo(0D) > 0 && UtilValidate.isEmpty(inventoryItem.get("locationSeqId")))) {
                            context.put("inventoryItem", inventoryItem);
                            context.put("reserveOisgirMap", reserveOisgirMap);
                            context.put("ebayReserveReasonEnumId", ebayReserveReasonEnumId);
                            context.put("createDetailMap", createDetailMap);
                            context.put("lastNonSerInventoryItem", lastNonSerInventoryItem);
                            reserveForInventoryItemInline(dctx, context);
                            context = (Map<String, Object>) context.get("context");
                            inlineResult = (Map<String, Object>) context.get("inlineResult");
                        }
                    }
                }
            }
            if (!java.util.Objects.equals(context.get("quantityNotReserved"), BigDecimal.ZERO)) {
                if ("Y".equals(context.get("requireInventory"))) {
                } else {
                    if (UtilValidate.isNotEmpty(lastNonSerInventoryItem)) {
                        createDetailMap.put("inventoryItemId", lastNonSerInventoryItem.get("inventoryItemId"));
                        createDetailMap.put("orderId", context.get("orderId"));
                        createDetailMap.put("orderItemSeqId", context.get("orderItemSeqId"));
                        createDetailMap.put("shipGroupSeqId", context.get("shipGroupSeqId"));
                        ((Map<String, Object>) createDetailMap).put("availableToPromiseDiff", new BigDecimal(context.get("quantityNotReserved").toString()));
                        if (UtilValidate.isNotEmpty(context.get("reserveReasonEnumId"))) {
                            createDetailMap.put("reasonEnumId", context.get("reserveReasonEnumId"));
                        }
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
                        try {
                            productFacility = lastNonSerInventoryItem.getRelatedOne("ProductFacility", false);
                        } catch (Exception e) {
                            Debug.logError(e, "Error getting related one ProductFacility: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                        daysToShip = null;
                        daysToShip = (Long) productFacility.get("daysToShip");
                        if (UtilValidate.isEmpty(daysToShip)) {
                            if (UtilValidate.isNotEmpty(facility.get("defaultDaysToShip"))) {
                                daysToShip = (Long) facility.get("defaultDaysToShip");
                            } else {
                                daysToShip = 30L;
                            }
                        }
                        try {
                            Map<String, Object> scriptContext = new HashMap<String, Object>();
                            scriptContext.put("delegator", delegator);
                            scriptContext.put("dispatcher", dispatcher);
                            scriptContext.put("locale", locale);
                            scriptContext.put("userLogin", userLogin);
                            scriptContext.put("context", context);
                            scriptContext.put("parameters", context);
                            Object scriptResult = GroovyUtil.eval("java.sql.Timestamp orderDate = orderHeader.getTimestamp(\"orderDate\")\n                            com.ibm.icu.util.Calendar cal = com.ibm.icu.util.Calendar.getInstance()\n                            cal.setTimeInMillis(orderDate.getTime())\n                            cal.add(com.ibm.icu.util.Calendar.DAY_OF_YEAR, daysToShip.intValue())\n                            return org.ofbiz.base.util.UtilMisc.toMap(\"promisedDatetime\", new java.sql.Timestamp(cal.getTimeInMillis()))", scriptContext);
                        } catch (Exception e) {
                            Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
                        }
                        reserveOisgirMap.put("orderId", context.get("orderId"));
                        reserveOisgirMap.put("orderItemSeqId", context.get("orderItemSeqId"));
                        reserveOisgirMap.put("shipGroupSeqId", context.get("shipGroupSeqId"));
                        reserveOisgirMap.put("inventoryItemId", lastNonSerInventoryItem.get("inventoryItemId"));
                        reserveOisgirMap.put("reserveOrderEnumId", context.get("reserveOrderEnumId"));
                        reserveOisgirMap.put("quantity", context.get("quantityNotReserved"));
                        reserveOisgirMap.put("quantityNotAvailable", context.get("quantityNotReserved"));
                        reserveOisgirMap.put("reservedDatetime", context.get("reservedDatetime"));
                        reserveOisgirMap.put("promisedDatetime", context.get("promisedDatetime"));
                        reserveOisgirMap.put("sequenceId", context.get("sequenceId"));
                        reserveOisgirMap.put("priority", context.get("priority"));
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("reserveOrderItemInventory", reserveOisgirMap);
                            if (ServiceUtil.isError(serviceResult)) {
                                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                            }
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling reserveOrderItemInventory: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                        reserveOisgirMap = new HashMap<String, Object>();
                    } else {
                        createInventoryItemInMap = null;
                        createInventoryItemOutMap = null;
                        try {
                            permUserLogin = EntityQuery.use(delegator)
                                    .from("UserLogin")
                                    .where(UtilMisc.toMap("userLoginId", "system"))
                                    .queryOne();
                        } catch (Exception e) {
                            Debug.logError(e, "Error querying UserLogin: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                        ((Map<String, Object>) createInventoryItemInMap).put("productId", context.get("productId"));
                        ((Map<String, Object>) createInventoryItemInMap).put("facilityId", context.get("facilityId"));
                        ((Map<String, Object>) createInventoryItemInMap).put("containerId", context.get("containerId"));
                        ((Map<String, Object>) createInventoryItemInMap).put("inventoryItemTypeId", "NON_SERIAL_INV_ITEM");
                        ((Map<String, Object>) createInventoryItemInMap).put("userLogin", permUserLogin);
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("createInventoryItem", (Map<String, Object>) createInventoryItemInMap);
                            if (ServiceUtil.isError(serviceResult)) {
                                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                            }
                            ((Map<String, Object>) createInventoryItemOutMap).put("inventoryItemId", serviceResult.get("inventoryItemId"));
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling createInventoryItem: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                        try {
                            newNonSerInventoryItem = EntityQuery.use(delegator)
                                    .from("InventoryItem")
                                    .where(UtilMisc.toMap("inventoryItemId", ((Map<String, Object>) createInventoryItemOutMap).get("inventoryItemId")))
                                    .queryOne();
                        } catch (Exception e) {
                            Debug.logError(e, "Error querying InventoryItem: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                        createDetailMap.put("inventoryItemId", newNonSerInventoryItem.get("inventoryItemId"));
                        createDetailMap.put("orderId", context.get("orderId"));
                        createDetailMap.put("orderItemSeqId", context.get("orderItemSeqId"));
                        createDetailMap.put("shipGroupSeqId", context.get("shipGroupSeqId"));
                        ((Map<String, Object>) createDetailMap).put("availableToPromiseDiff", new BigDecimal(context.get("quantityNotReserved").toString()));
                        if (UtilValidate.isNotEmpty(context.get("reserveReasonEnumId"))) {
                            createDetailMap.put("reasonEnumId", context.get("reserveReasonEnumId"));
                        }
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
                        try {
                            productFacility = newNonSerInventoryItem.getRelatedOne("ProductFacility", false);
                        } catch (Exception e) {
                            Debug.logError(e, "Error getting related one ProductFacility: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                        daysToShip = null;
                        daysToShip = (Long) productFacility.get("daysToShip");
                        if (UtilValidate.isEmpty(daysToShip)) {
                            if (UtilValidate.isNotEmpty(facility.get("defaultDaysToShip"))) {
                                daysToShip = (Long) facility.get("defaultDaysToShip");
                            } else {
                                daysToShip = 30L;
                            }
                        }
                        try {
                            Map<String, Object> scriptContext = new HashMap<String, Object>();
                            scriptContext.put("delegator", delegator);
                            scriptContext.put("dispatcher", dispatcher);
                            scriptContext.put("locale", locale);
                            scriptContext.put("userLogin", userLogin);
                            scriptContext.put("context", context);
                            scriptContext.put("parameters", context);
                            Object scriptResult = GroovyUtil.eval("java.sql.Timestamp orderDate = orderHeader.getTimestamp(\"orderDate\")\n                            com.ibm.icu.util.Calendar cal = com.ibm.icu.util.Calendar.getInstance()\n                            cal.setTimeInMillis(orderDate.getTime())\n                            cal.add(com.ibm.icu.util.Calendar.DAY_OF_YEAR, daysToShip.intValue())\n                            return org.ofbiz.base.util.UtilMisc.toMap(\"promisedDatetime\", new java.sql.Timestamp(cal.getTimeInMillis()))", scriptContext);
                        } catch (Exception e) {
                            Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
                        }
                        reserveOisgirMap.put("orderId", context.get("orderId"));
                        reserveOisgirMap.put("orderItemSeqId", context.get("orderItemSeqId"));
                        reserveOisgirMap.put("shipGroupSeqId", context.get("shipGroupSeqId"));
                        reserveOisgirMap.put("inventoryItemId", newNonSerInventoryItem.get("inventoryItemId"));
                        reserveOisgirMap.put("reserveOrderEnumId", context.get("reserveOrderEnumId"));
                        reserveOisgirMap.put("quantity", context.get("quantityNotReserved"));
                        reserveOisgirMap.put("quantityNotAvailable", context.get("quantityNotReserved"));
                        reserveOisgirMap.put("reservedDatetime", context.get("reservedDatetime"));
                        reserveOisgirMap.put("promisedDatetime", context.get("promisedDatetime"));
                        reserveOisgirMap.put("sequenceId", context.get("sequenceId"));
                        reserveOisgirMap.put("priority", context.get("priority"));
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("reserveOrderItemInventory", reserveOisgirMap);
                            if (ServiceUtil.isError(serviceResult)) {
                                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                            }
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling reserveOrderItemInventory: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                        reserveOisgirMap = new HashMap<String, Object>();
                    }
                    context.put("quantityNotReserved", BigDecimal.ZERO);
                }
            }
        }
        result.put("quantityNotReserved", context.get("quantityNotReserved"));

        return result;
    }


    /**
     * Reserve a Specific Serialized InventoryItem
     */
    public static Map<String, Object> reserveAnInventoryItem(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> createDetailMap = null;
        GenericValue inventoryItem = null;
        Map<String, Object> inventoryItemLookupPk = null;
        Map<String, Object> receiveCtx = new HashMap<>();
        Map<String, Object> cancelOrderItemShipGrpInvResMap = null;
        Map<String, Object> reserveOisgirMap = null;
        try {
            inventoryItem = EntityQuery.use(delegator)
                    .from("InventoryItem")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying InventoryItem: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Object facilityId = inventoryItem.get("facilityId");
        Map<String, Object> inventoryReservationLookUp = new HashMap<String, Object>();
        inventoryReservationLookUp.put("inventoryItemId", inventoryItem.get("inventoryItemId"));
        if ("NON_SERIAL_INV_ITEM".equals(inventoryItem.get("inventoryItemTypeId"))) {
            createDetailMap.put("inventoryItemId", inventoryItem.get("inventoryItemId"));
            createDetailMap.put("orderId", context.get("orderId"));
            createDetailMap.put("orderItemSeqId", context.get("orderItemSeqId"));
            createDetailMap.put("quantityOnHandDiff", new BigDecimal("-1"));
            createDetailMap.put("availableToPromiseDiff", new BigDecimal("-1"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createInventoryItemDetail", createDetailMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling createInventoryItemDetail: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        // set-service-fields from "parameters" to "cancelOrderItemShipGrpInvResMap" for service "cancelOrderItemShipGrpInvRes"
        cancelOrderItemShipGrpInvResMap.putAll(UtilMisc.toMap(context));
        cancelOrderItemShipGrpInvResMap.put("cancelQuantity", context.get("quantity"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("cancelOrderItemShipGrpInvRes", cancelOrderItemShipGrpInvResMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling cancelOrderItemShipGrpInvRes: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        List<GenericValue> inventoryItems = null;
        try {
            inventoryItems = EntityQuery.use(delegator)
                    .from("InventoryItem")
                    .where(UtilMisc.toMap("productId", context.get("productId"), "inventoryItemTypeId", "SERIALIZED_INV_ITEM", "serialNumber", context.get("serialNumber")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        inventoryItem = null;
        inventoryItem = EntityUtil.getFirst((List<GenericValue>) inventoryItems);
        if (UtilValidate.isEmpty(inventoryItem)) {
            receiveCtx.put("productId", context.get("productId"));
            receiveCtx.put("facilityId", facilityId);
            receiveCtx.put("quantityAccepted", context.get("quantity"));
            receiveCtx.put("quantityRejected", BigDecimal.ZERO);
            receiveCtx.put("inventoryItemTypeId", "SERIALIZED_INV_ITEM");
            receiveCtx.put("serialNumber", context.get("serialNumber"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("receiveInventoryProduct", receiveCtx);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
                inventoryItemLookupPk.put("inventoryItemId", serviceResult.get("inventoryItemId"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling receiveInventoryProduct: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            try {
                inventoryItem = EntityQuery.use(delegator)
                        .from("InventoryItem")
                        .where(inventoryItemLookupPk)
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error finding by primary key InventoryItem: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        inventoryReservationLookUp.put("inventoryItemId", inventoryItem.get("inventoryItemId"));
        List<GenericValue> invReservations = null;
        try {
            invReservations = EntityQuery.use(delegator)
                    .from("OrderItemShipGrpInvRes")
                    .where(inventoryReservationLookUp)
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderItemShipGrpInvRes: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue inventoryItemReservation = EntityUtil.getFirst((List<GenericValue>) invReservations);
        if (UtilValidate.isNotEmpty(inventoryItemReservation)) {
            // set-service-fields from "inventoryItemReservation" to "cancelOrderItemShipGrpInvResMap" for service "cancelOrderItemShipGrpInvRes"
            cancelOrderItemShipGrpInvResMap.putAll(UtilMisc.toMap(inventoryItemReservation));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("cancelOrderItemShipGrpInvRes", cancelOrderItemShipGrpInvResMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling cancelOrderItemShipGrpInvRes: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            try {
                inventoryItem.refresh();
            } catch (Exception e) {
                Debug.logError(e, "Error refreshing value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            inventoryItem.put("statusId", "INV_PROMISED");
            try {
                delegator.store(inventoryItem);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            reserveOisgirMap.put("orderId", inventoryItemReservation.get("orderId"));
            reserveOisgirMap.put("productId", context.get("productId"));
            reserveOisgirMap.put("orderItemSeqId", inventoryItemReservation.get("orderItemSeqId"));
            reserveOisgirMap.put("shipGroupSeqId", inventoryItemReservation.get("shipGroupSeqId"));
            reserveOisgirMap.put("reserveOrderEnumId", inventoryItemReservation.get("reserveOrderEnumId"));
            reserveOisgirMap.put("reservedDatetime", inventoryItemReservation.get("reservedDatetime"));
            reserveOisgirMap.put("quantity", BigDecimal.ONE);
            reserveOisgirMap.put("requireInventory", context.get("requireInventory"));
            if (UtilValidate.isNotEmpty(inventoryItemReservation.get("sequenceId"))) {
                reserveOisgirMap.put("sequenceId", inventoryItemReservation.get("sequenceId"));
            }
            reserveOisgirMap.put("priority", context.get("priority"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("reserveProductInventory", reserveOisgirMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling reserveProductInventory: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            reserveOisgirMap = new HashMap<String, Object>();
        }
        if ("INV_AVAILABLE".equals(inventoryItem.get("statusId"))) {
            inventoryItem.put("statusId", "INV_PROMISED");
            try {
                delegator.store(inventoryItem);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        reserveOisgirMap.put("orderId", context.get("orderId"));
        reserveOisgirMap.put("orderItemSeqId", context.get("orderItemSeqId"));
        reserveOisgirMap.put("shipGroupSeqId", context.get("shipGroupSeqId"));
        reserveOisgirMap.put("inventoryItemId", inventoryItem.get("inventoryItemId"));
        reserveOisgirMap.put("reserveOrderEnumId", context.get("reserveOrderEnumId"));
        reserveOisgirMap.put("reservedDatetime", context.get("reservedDatetime"));
        reserveOisgirMap.put("promisedDatetime", context.get("promisedDatetime"));
        reserveOisgirMap.put("quantity", BigDecimal.ONE);
        if (UtilValidate.isNotEmpty(context.get("sequenceId"))) {
            reserveOisgirMap.put("sequenceId", context.get("sequenceId"));
        }
        reserveOisgirMap.put("priority", context.get("priority"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("reserveOrderItemInventory", reserveOisgirMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling reserveOrderItemInventory: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        reserveOisgirMap = new HashMap<String, Object>();
        result.put("inventoryItemId", inventoryItem.get("inventoryItemId"));

        return result;
    }


    /**
     * Does a reservation for one InventoryItem, meant to be called in-line
     */
    public static Map<String, Object> reserveForInventoryItemInline(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue inventoryItem = null;
        Map<String, Object> reserveOisgirMap = null;
        Object ebayReserveReasonEnumId = null;
        Map<String, Object> inlineResult = null;
        Map<String, Object> createDetailMap = null;
        Object lastNonSerInventoryItem = null;
        GenericValue productFacility = null;
        Long daysToShip = null;
        if (((Comparable) context.get("quantityNotReserved")).compareTo(BigDecimal.ZERO) > 0) {
            if ("SERIALIZED_INV_ITEM".equals(inventoryItem.get("inventoryItemTypeId"))) {
                if ("INV_AVAILABLE".equals(inventoryItem.get("statusId"))) {
                    inventoryItem.put("statusId", "INV_PROMISED");
                    try {
                        delegator.store(inventoryItem);
                    } catch (Exception e) {
                        Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    inlineResult = getPromisedDateTime(dctx, context);
                    if (ServiceUtil.isError(inlineResult)) {
                        return inlineResult;
                    }
                    reserveOisgirMap.put("orderId", context.get("orderId"));
                    reserveOisgirMap.put("orderItemSeqId", context.get("orderItemSeqId"));
                    reserveOisgirMap.put("shipGroupSeqId", context.get("shipGroupSeqId"));
                    reserveOisgirMap.put("inventoryItemId", inventoryItem.get("inventoryItemId"));
                    reserveOisgirMap.put("reserveOrderEnumId", context.get("reserveOrderEnumId"));
                    reserveOisgirMap.put("reservedDatetime", context.get("reservedDatetime"));
                    reserveOisgirMap.put("promisedDatetime", context.get("promisedDatetime"));
                    reserveOisgirMap.put("quantity", BigDecimal.ONE);
                    if (UtilValidate.isNotEmpty(context.get("sequenceId"))) {
                        reserveOisgirMap.put("sequenceId", context.get("sequenceId"));
                    }
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("reserveOrderItemInventory", reserveOisgirMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling reserveOrderItemInventory: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    reserveOisgirMap = new HashMap<String, Object>();
                    ((Map<String, Object>) context).put("quantityNotReserved", new BigDecimal(context.get("quantityNotReserved").toString()));
                }
            }
            if ("NON_SERIAL_INV_ITEM".equals(inventoryItem.get("inventoryItemTypeId"))) {
                if (UtilValidate.isNotEmpty(context.get("reserveReasonEnumId"))) {
                    if ("EBAY_INV_RES".equals(context.get("reserveReasonEnumId"))) {
                        ebayReserveReasonEnumId = context.get("reserveReasonEnumId");
                    }
                }
                Object parameters_deductAmount = null;
                Object createDetailMap_inventoryItemId = null;
                Object createDetailMap_orderId = null;
                Object createDetailMap_orderItemSeqId = null;
                Object createDetailMap_reasonEnumId = null;
                Object reserveOisgirMap_orderId = null;
                Object reserveOisgirMap_orderItemSeqId = null;
                Object reserveOisgirMap_shipGroupSeqId = null;
                Object reserveOisgirMap_inventoryItemId = null;
                Object reserveOisgirMap_reserveOrderEnumId = null;
                Object reserveOisgirMap_reservedDatetime = null;
                BigDecimal reserveOisgirMap_quantity = null;
                Object reserveOisgirMap_promisedDatetime = null;
                Object reserveOisgirMap_priority = null;
                Object reserveOisgirMap_sequenceId = null;
                if ((!"INV_NS_ON_HOLD".equals(inventoryItem.get("statusId")) && !"INV_NS_DEFECTIVE".equals(inventoryItem.get("statusId")) && !(UtilValidate.isEmpty(inventoryItem.get("availableToPromiseTotal"))) && ((Comparable) inventoryItem.get("availableToPromiseTotal")).compareTo(BigDecimal.ZERO) > 0)) {
                    if (context.get("quantityNotReserved") != null /* TODO: field compare operator greater */) {
                        context.put("deductAmount", inventoryItem.get("availableToPromiseTotal"));
                    } else {
                        context.put("deductAmount", context.get("quantityNotReserved"));
                    }
                    createDetailMap.put("inventoryItemId", inventoryItem.get("inventoryItemId"));
                    createDetailMap.put("orderId", context.get("orderId"));
                    createDetailMap.put("orderItemSeqId", context.get("orderItemSeqId"));
                    ((Map<String, Object>) createDetailMap).put("availableToPromiseDiff", new BigDecimal(context.get("deductAmount").toString()));
                    if (UtilValidate.isNotEmpty(ebayReserveReasonEnumId)) {
                        createDetailMap.put("reasonEnumId", context.get("reserveReasonEnumId"));
                    }
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
                    if (UtilValidate.isEmpty(ebayReserveReasonEnumId)) {
                        inlineResult = getPromisedDateTime(dctx, context);
                        if (ServiceUtil.isError(inlineResult)) {
                            return inlineResult;
                        }
                        reserveOisgirMap.put("orderId", context.get("orderId"));
                        reserveOisgirMap.put("orderItemSeqId", context.get("orderItemSeqId"));
                        reserveOisgirMap.put("shipGroupSeqId", context.get("shipGroupSeqId"));
                        reserveOisgirMap.put("inventoryItemId", inventoryItem.get("inventoryItemId"));
                        reserveOisgirMap.put("reserveOrderEnumId", context.get("reserveOrderEnumId"));
                        reserveOisgirMap.put("reservedDatetime", context.get("reservedDatetime"));
                        reserveOisgirMap.put("quantity", context.get("deductAmount"));
                        reserveOisgirMap.put("promisedDatetime", context.get("promisedDatetime"));
                        reserveOisgirMap.put("priority", context.get("priority"));
                        if (UtilValidate.isNotEmpty(context.get("sequenceId"))) {
                            reserveOisgirMap.put("sequenceId", context.get("sequenceId"));
                        }
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("reserveOrderItemInventory", reserveOisgirMap);
                            if (ServiceUtil.isError(serviceResult)) {
                                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                            }
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling reserveOrderItemInventory: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                        reserveOisgirMap = new HashMap<String, Object>();
                    }
                    ((Map<String, Object>) context).put("quantityNotReserved", new BigDecimal(context.get("deductAmount").toString()));
                }
                lastNonSerInventoryItem = inventoryItem;
            }
        }

        return result;
    }


    /**
     * Get Inventory Promised Date/Time
     */
    public static Map<String, Object> getPromisedDateTime(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Long daysToShip = null;
        GenericValue productFacility = null;
        try {
            productFacility = ((GenericValue) context.get("inventoryItem")).getRelatedOne("ProductFacility", false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related one ProductFacility: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        daysToShip = (Long) productFacility.get("daysToShip");
        if (UtilValidate.isEmpty(daysToShip)) {
            daysToShip = (Long) ((Map<String, Object>) context.get("facility")).get("defaultDaysToShip");
        }
        if (UtilValidate.isEmpty(daysToShip)) {
            daysToShip = 30L;
        }
        try {
            Map<String, Object> scriptContext = new HashMap<String, Object>();
            scriptContext.put("delegator", delegator);
            scriptContext.put("dispatcher", dispatcher);
            scriptContext.put("locale", locale);
            scriptContext.put("userLogin", userLogin);
            scriptContext.put("context", context);
            scriptContext.put("parameters", context);
            Object scriptResult = GroovyUtil.eval("java.sql.Timestamp orderDate = orderHeader.getTimestamp(\"orderDate\")\n        com.ibm.icu.util.Calendar cal = com.ibm.icu.util.Calendar.getInstance()\n        cal.setTimeInMillis(orderDate.getTime())\n        cal.add(com.ibm.icu.util.Calendar.DAY_OF_YEAR, daysToShip.intValue())\n        return org.ofbiz.base.util.UtilMisc.toMap(\"promisedDatetime\", new java.sql.Timestamp(cal.getTimeInMillis()))", scriptContext);
        } catch (Exception e) {
            Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
        }

        return result;
    }


    /**
     * Reserve Order Item Inventory
     */
    public static Map<String, Object> reserveOrderItemInventory(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue newOisgirEntity = null;
        Timestamp nowTimestamp = null;
        GenericValue checkOisgirEntity = null;
        try {
            checkOisgirEntity = EntityQuery.use(delegator)
                    .from("OrderItemShipGrpInvRes")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderItemShipGrpInvRes: " + e.getMessage(), MODULE);
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
        context.put("promisedDatetime", orderItem.get("shipBeforeDate"));
        context.put("currentPromisedDate", orderItem.get("shipBeforeDate"));
        if (UtilValidate.isEmpty(checkOisgirEntity)) {
            newOisgirEntity = delegator.makeValue("OrderItemShipGrpInvRes");
            newOisgirEntity.setPKFields(context);
            newOisgirEntity.setNonPKFields(context);
            nowTimestamp = new Timestamp(System.currentTimeMillis());
            newOisgirEntity.put("createdDatetime", nowTimestamp);
            newOisgirEntity.put("priority", context.get("priority"));
            if (UtilValidate.isEmpty(newOisgirEntity.get("reservedDatetime"))) {
                newOisgirEntity.put("reservedDatetime", nowTimestamp);
            }
            try {
                delegator.create(newOisgirEntity);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        } else {
            checkOisgirEntity.set("quantity", new BigDecimal(checkOisgirEntity.get("quantity").toString()));
            checkOisgirEntity.set("quantityNotAvailable", new BigDecimal(checkOisgirEntity.get("quantityNotAvailable").toString()));
            try {
                delegator.store(checkOisgirEntity);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        return result;
    }


    /**
     * Cancel Inventory Reservation for an Order
     */
    public static Map<String, Object> cancelOrderInventoryReservation(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> oisgirListLookupMap = null;
        Map<String, Object> cancelOisgirMap = null;
        Map<String, Object> checkDiiMap = null;
        oisgirListLookupMap.put("orderId", context.get("orderId"));
        if (UtilValidate.isNotEmpty(context.get("orderItemSeqId"))) {
            oisgirListLookupMap.put("orderItemSeqId", context.get("orderItemSeqId"));
            Debug.logVerbose("OISGIR Cancel for single item : " + oisgirListLookupMap, MODULE);
        }
        if (UtilValidate.isNotEmpty(context.get("shipGroupSeqId"))) {
            oisgirListLookupMap.put("shipGroupSeqId", context.get("shipGroupSeqId"));
            Debug.logVerbose("OISGIR Cancel for single item : " + oisgirListLookupMap, MODULE);
        }
        List<GenericValue> oisgirList = null;
        try {
            oisgirList = EntityQuery.use(delegator)
                    .from("OrderItemShipGrpInvRes")
                    .where(oisgirListLookupMap)
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderItemShipGrpInvRes: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (oisgirList != null) {
            for (GenericValue oisgir : oisgirList) {
                cancelOisgirMap.put("orderId", oisgir.get("orderId"));
                cancelOisgirMap.put("orderItemSeqId", oisgir.get("orderItemSeqId"));
                cancelOisgirMap.put("shipGroupSeqId", oisgir.get("shipGroupSeqId"));
                cancelOisgirMap.put("inventoryItemId", oisgir.get("inventoryItemId"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("cancelOrderItemShipGrpInvRes", cancelOisgirMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling cancelOrderItemShipGrpInvRes: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                checkDiiMap.put("inventoryItemId", oisgir.get("inventoryItemId"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("checkDecomposeInventoryItem", checkDiiMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling checkDecomposeInventoryItem: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }

        return result;
    }


    /**
     * Cancel Inventory Reservation Qty For An Item
     */
    public static Map<String, Object> cancelOrderItemInvResQty(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> cancelMap = null;
        Map<String, Object> cancelOisgirMap = null;
        Map<String, Object> checkDiiMap = null;
        Object toCancelAmount = null;
        Map<String, Object> oisgirListLookupMap = null;
        List<GenericValue> oisgirList = null;
        if (UtilValidate.isEmpty(context.get("cancelQuantity"))) {
            cancelMap.put("orderId", context.get("orderId"));
            cancelMap.put("orderItemSeqId", context.get("orderItemSeqId"));
            cancelMap.put("shipGroupSeqId", context.get("shipGroupSeqId"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("cancelOrderInventoryReservation", cancelMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling cancelOrderInventoryReservation: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        if (UtilValidate.isNotEmpty(context.get("cancelQuantity"))) {
            toCancelAmount = context.get("cancelQuantity");
            oisgirListLookupMap.put("orderId", context.get("orderId"));
            oisgirListLookupMap.put("orderItemSeqId", context.get("orderItemSeqId"));
            oisgirListLookupMap.put("shipGroupSeqId", context.get("shipGroupSeqId"));
            try {
                oisgirList = EntityQuery.use(delegator)
                        .from("OrderItemShipGrpInvRes")
                        .where(oisgirListLookupMap)
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying OrderItemShipGrpInvRes: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (oisgirList != null) {
                for (GenericValue oisgir : oisgirList) {
                    if (((Comparable) toCancelAmount).compareTo(BigDecimal.ZERO) > 0) {
                        if (oisgir.get("quantity") != null /* TODO: field compare operator greater-equals */) {
                            cancelOisgirMap.put("cancelQuantity", toCancelAmount);
                        }
                        if (oisgir.get("quantity") != null /* TODO: field compare operator less */) {
                            cancelOisgirMap.put("cancelQuantity", oisgir.get("quantity"));
                        }
                        cancelOisgirMap.put("orderId", oisgir.get("orderId"));
                        cancelOisgirMap.put("orderItemSeqId", oisgir.get("orderItemSeqId"));
                        cancelOisgirMap.put("shipGroupSeqId", oisgir.get("shipGroupSeqId"));
                        cancelOisgirMap.put("inventoryItemId", oisgir.get("inventoryItemId"));
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("cancelOrderItemShipGrpInvRes", cancelOisgirMap);
                            if (ServiceUtil.isError(serviceResult)) {
                                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                            }
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling cancelOrderItemShipGrpInvRes: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                        checkDiiMap.put("inventoryItemId", oisgir.get("inventoryItemId"));
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("checkDecomposeInventoryItem", checkDiiMap);
                            if (ServiceUtil.isError(serviceResult)) {
                                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                            }
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling checkDecomposeInventoryItem: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                        toCancelAmount = new BigDecimal(((Map<String, Object>) cancelOisgirMap).get("cancelQuantity").toString());
                    }
                }
            }
        }

        return result;
    }


    /**
     * Cancel An Inventory Reservation
     */
    public static Map<String, Object> cancelOrderItemShipGrpInvRes(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue inventoryItem = null;
        Object cancelQuantity = null;
        Map<String, Object> createDetailMap = null;
        GenericValue orderItemShipGrpInvRes = null;
        try {
            orderItemShipGrpInvRes = EntityQuery.use(delegator)
                    .from("OrderItemShipGrpInvRes")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderItemShipGrpInvRes: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            inventoryItem = orderItemShipGrpInvRes.getRelatedOne("InventoryItem", false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related one InventoryItem: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if ("SERIALIZED_INV_ITEM".equals(inventoryItem.get("inventoryItemTypeId"))) {
            Debug.logVerbose("Serialized inventory re-enabled.", MODULE);
            inventoryItem.put("statusId", "INV_AVAILABLE");
            try {
                delegator.removeValue(orderItemShipGrpInvRes);
            } catch (Exception e) {
                Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            try {
                delegator.store(inventoryItem);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        if ("NON_SERIAL_INV_ITEM".equals(inventoryItem.get("inventoryItemTypeId"))) {
            Debug.logVerbose("Non-Serialized inventory item incrementing availableToPromise.", MODULE);
            cancelQuantity = context.get("cancelQuantity");
            if (UtilValidate.isEmpty(cancelQuantity)) {
                cancelQuantity = orderItemShipGrpInvRes.get("quantity");
            }
            createDetailMap.put("inventoryItemId", inventoryItem.get("inventoryItemId"));
            createDetailMap.put("orderId", context.get("orderId"));
            createDetailMap.put("orderItemSeqId", context.get("orderItemSeqId"));
            createDetailMap.put("shipGroupSeqId", context.get("shipGroupSeqId"));
            createDetailMap.put("availableToPromiseDiff", cancelQuantity);
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
            if (cancelQuantity != null /* TODO: field compare operator less */) {
                orderItemShipGrpInvRes.set("quantity", new BigDecimal(cancelQuantity.toString()));
                try {
                    delegator.store(orderItemShipGrpInvRes);
                } catch (Exception e) {
                    Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            } else {
                try {
                    delegator.removeValue(orderItemShipGrpInvRes);
                } catch (Exception e) {
                    Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }

        return result;
    }

}

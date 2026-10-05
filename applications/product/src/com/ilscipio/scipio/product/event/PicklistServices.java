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
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.security.Security;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://product/script/org/ofbiz/shipment/picklist/PicklistServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class PicklistServices {

    private static final String MODULE = PicklistServices.class.getName();


    /**
     * Find Orders Ready to Pick or that need Stock Moves
     */
    public static Map<String, Object> findOrdersToPickMove(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<GenericValue> orderHeaderList = null;
        List<GenericValue> orderItemAndShipGroupAssocList = null;
        GenericValue inventoryItem = null;
        Object needsStockMove = null;
        List<GenericValue> orderItemShipGroupList = null;
        Object locationCount = null;
        List<GenericValue> orderItemShipGrpInvResList = null;
        List<Object> rushOrderInfo_orderNeedsStockMoveInfoList = null;
        Map<String, Object> pickMoveInfoMap = null;
        BigDecimal remainingQuantityToBePicked = null;
        Long orderItemCount = null;
        Object pickThisOrder = null;
        String noOfOrderItems = null;
        Object numberSoFar = null;
        List<GenericValue> OrderHeaderAndItemFacilityLocationList = null;
        List<Object> finalOrderItemShipGrpInvResList = null;
        List<Object> pickMoveInfoMap_groupName__orderReadyToPickInfoList = null;
        GenericValue orderItem = null;
        Map<String, Object> orderHeaderInfo = null;
        Object pickedItemQuantity = null;
        List<GenericValue> picklistItemList = null;
        List<Object> pickMoveInfoMap_groupName__orderNeedsStockMoveInfoList = null;
        Object allPickStarted = null;
        List<Object> orderItemShipGrpInvResInfoList = null;
        Object groupName3 = null;
        List<Object> groupNames = null;
        String groupName2 = null;
        GenericValue facilityLocation = null;
        Map<String, Object> orderItemShipGrpInvResInfo = null;
        Object hasStockToPick = null;
        List<Object> locations = null;
        List<Object> rushOrderInfo_orderReadyToPickInfoList = null;
        Object groupName1 = null;
        Object locationGroupName = null;
        List<Object> pickMoveInfoList = null;
        Object groupByShippingMethod = context.get("groupByShippingMethod");
        Object groupByNoOfOrderItems = context.get("groupByNoOfOrderItems");
        Object groupByWarehouseArea = context.get("groupByWarehouseArea");
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        GenericValue orderHeader = null;
        if (UtilValidate.isNotEmpty(context.get("orderId"))) {
            try {
                orderHeader = EntityQuery.use(delegator)
                        .from("OrderHeader")
                        .where(context)
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying OrderHeader: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            orderHeaderList.add(orderHeader);
        } else {
            if (UtilValidate.isEmpty(context.get("orderHeaderList"))) {
                Debug.logInfo("No order header list found in parameters; finding orders to pick.", MODULE);
                try {
                    orderHeaderList = EntityQuery.use(delegator)
                            .from("OrderHeader")
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying OrderHeader: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            } else {
                orderHeaderList = (List<GenericValue>) context.get("orderHeaderList");
                Debug.logInfo("Found orderHeaderList in parameters; using: " + orderHeaderList, MODULE);
            }
        }
        Long maxNumberOfOrders = (Long) context.get("maxNumberOfOrders");
        numberSoFar = 0L;
        if (orderHeaderList != null) {
            for (GenericValue orderHeaderEntry : orderHeaderList) {
                Debug.logInfo("Checking order #" + orderHeaderEntry.get("orderId") + " to add to picklist", MODULE);
                try {
                    orderItemShipGroupList = EntityQuery.use(delegator)
                            .from("OrderItemShipGroup")
                            .where(UtilMisc.toMap("orderId", orderHeaderEntry.get("orderId")))
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                orderItemCount = null;
                try {
                    orderItemCount = EntityQuery.use(delegator)
                            .from("OrderItem")
                            .where("orderId", orderHeaderEntry.get("orderId"))
                            .queryCount();
                } catch (Exception e) {
                    Debug.logError(e, "Error counting OrderItem: " + e.getMessage(), MODULE);
                }
                Object groupName = null;
                groupName1 = null;
                groupName2 = null;
                groupName3 = null;
                if ((UtilValidate.isEmpty(groupByShippingMethod) && UtilValidate.isEmpty(groupByWarehouseArea) && UtilValidate.isEmpty(groupByNoOfOrderItems))) {
                    groupName = orderHeaderEntry.get("orderId");
                } else {
                    try {
                        OrderHeaderAndItemFacilityLocationList = EntityQuery.use(delegator)
                                .from("OrderHeaderAndItemFacilityLocation")
                                .distinct()
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying OrderHeaderAndItemFacilityLocation: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    if (OrderHeaderAndItemFacilityLocationList != null) {
                        for (GenericValue orderHeaderAndItemFacilityLocation : OrderHeaderAndItemFacilityLocationList) {
                            if ("Y".equals(groupByShippingMethod)) {
                                groupName1 = orderHeaderAndItemFacilityLocation.get("shipmentMethodTypeId");
                            }
                            if ("Y".equals(groupByWarehouseArea)) {
                                groupName2 = (String) orderHeaderAndItemFacilityLocation.get("areaId");
                                locationGroupName = orderHeaderAndItemFacilityLocation.get("areaId");
                            }
                            if (("Y".equals(groupByNoOfOrderItems) && ((Comparable) orderItemCount).compareTo(new BigDecimal("3")) < 0)) {
                                noOfOrderItems = UtilProperties.getMessage("ProductUiLabels", "FacilityNumberOfItemsLessThanThree", locale);
                                groupName3 = "Items_Less_Than_3";
                            } else if (("Y".equals(groupByNoOfOrderItems) && ((Comparable) orderItemCount).compareTo(new BigDecimal("3")) >= 0)) {
                                noOfOrderItems = UtilProperties.getMessage("ProductUiLabels", "FacilityNumberOfItemsThreeOrMore", locale);
                                groupName3 = "Items_Three_Or_More";
                            }
                            groupName = "" + groupName1 + groupName2 + groupName3;
                            if (("Y".equals(groupByWarehouseArea) && !(UtilValidate.isEmpty(locationGroupName)) && !(locations != null /* TODO: field compare operator contains */))) {
                                locations.add(locationGroupName);
                            }
                        }
                    }
                    locationCount = "0";
                    if (UtilValidate.isNotEmpty(locations)) {
                        locationCount = (Long) (long) (locations != null ? ((java.util.List<?>) locations).size() : 0);
                    }
                    if (((Comparable) locationCount).compareTo(1L) > 0) {
                        groupName2 = "MULTI_LOCATIONS";
                        groupName = "" + groupName1 + groupName2 + groupName3;
                        groupName2 = UtilProperties.getMessage("ProductUiLabels", "FacilityMultipleLocations", locale);
                    }
                    locations = null;
                    locationCount = null;
                }
                if (orderItemShipGroupList != null) {
                    for (GenericValue orderItemShipGroup : orderItemShipGroupList) {
                        GenericValue orderItemShipGrpInvRes = null;
                        Object orderItemShipGrpInvRes_quantity = null;
                        Object orderItemShipGrpInvResInfo_orderItemShipGrpInvRes = null;
                        Object orderItemShipGrpInvResInfo_inventoryItem = null;
                        Object orderItemShipGrpInvResInfo_facilityLocation = null;
                        Object orderHeaderInfo_orderHeader = null;
                        Object orderHeaderInfo_orderItemShipGroup = null;
                        Object orderHeaderInfo_orderItemAndShipGroupAssocList = null;
                        Object orderHeaderInfo_orderItemShipGrpInvResList = null;
                        Object orderHeaderInfo_orderItemShipGrpInvResInfoList = null;
                        Object pickMoveInfoMap_groupName__groupName = null;
                        Object pickMoveInfoMap_groupName__groupName1 = null;
                        Object pickMoveInfoMap_groupName__groupName2 = null;
                        Object pickMoveInfoMap_groupName__groupName3 = null;
                        if ((UtilValidate.isEmpty(orderItemShipGroup.get("shipAfterDate")) || nowTimestamp != null /* TODO: field compare operator greater-equals */)) {
                            try {
                                orderItemShipGrpInvResList = EntityQuery.use(delegator)
                                        .from("OrderItemShipGrpInvRes")
                                        .where(UtilMisc.toMap("orderId", orderItemShipGroup.get("orderId"), "shipGroupSeqId", orderItemShipGroup.get("shipGroupSeqId")))
                                        .queryList();
                            } catch (Exception e) {
                                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                                return ServiceUtil.returnError(e.getMessage());
                            }
                            try {
                                orderItemAndShipGroupAssocList = EntityQuery.use(delegator)
                                        .from("OrderItemAndShipGroupAssoc")
                                        .where(UtilMisc.toMap("orderId", orderItemShipGroup.get("orderId"), "shipGroupSeqId", orderItemShipGroup.get("shipGroupSeqId")))
                                        .queryList();
                            } catch (Exception e) {
                                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                                return ServiceUtil.returnError(e.getMessage());
                            }
                            pickThisOrder = "Y";
                            needsStockMove = "N";
                            allPickStarted = "Y";
                            hasStockToPick = "N";
                            if (orderItemShipGrpInvResList != null) {
                                for (GenericValue orderItemShipGrpInvResEntry : orderItemShipGrpInvResList) {
                                    try {
                                        orderItem = orderItemShipGrpInvResEntry.getRelatedOne("OrderItem", false);
                                    } catch (Exception e) {
                                        Debug.logError(e, "Error getting related one OrderItem: " + e.getMessage(), MODULE);
                                        return ServiceUtil.returnError(e.getMessage());
                                    }
                                    if (!"ITEM_APPROVED".equals(orderItem.get("statusId"))) {
                                        pickThisOrder = "N";
                                    }
                                    if ("Y".equals(pickThisOrder)) {
                                        try {
                                            inventoryItem = orderItemShipGrpInvResEntry.getRelatedOne("InventoryItem", false);
                                        } catch (Exception e) {
                                            Debug.logError(e, "Error getting related one InventoryItem: " + e.getMessage(), MODULE);
                                            return ServiceUtil.returnError(e.getMessage());
                                        }
                                        try {
                                            picklistItemList = EntityQuery.use(delegator)
                                                    .from("PicklistAndBinAndItem")
                                                    .queryList();
                                        } catch (Exception e) {
                                            Debug.logError(e, "Error querying PicklistAndBinAndItem: " + e.getMessage(), MODULE);
                                            return ServiceUtil.returnError(e.getMessage());
                                        }
                                        Debug.logInfo("Pick list ITEMS - " + picklistItemList, MODULE);
                                        pickedItemQuantity = BigDecimal.ZERO;
                                        if (picklistItemList != null) {
                                            for (GenericValue picklistItem : picklistItemList) {
                                                pickedItemQuantity = new BigDecimal(picklistItem.get("quantity").toString());
                                            }
                                        }
                                        remainingQuantityToBePicked = (new BigDecimal(orderItemShipGrpInvResEntry.get("quantity").toString())).subtract(new BigDecimal(pickedItemQuantity.toString()));
                                        if (((Comparable) remainingQuantityToBePicked).compareTo(BigDecimal.ZERO) > 0) {
                                            orderItemShipGrpInvResEntry.put("quantity", remainingQuantityToBePicked);
                                            Debug.logInfo("The pick list item list is empty!", MODULE);
                                            allPickStarted = "N";
                                            if ((("N".equals(orderItemShipGroup.get("maySplit")) && !(UtilValidate.isEmpty(orderItemShipGrpInvResEntry.get("quantityNotAvailable"))) && ((Comparable) orderItemShipGrpInvResEntry.get("quantityNotAvailable")).compareTo(BigDecimal.ZERO) > 0) || !java.util.Objects.equals(context.get("facilityId"), inventoryItem.get("facilityId")))) {
                                                pickThisOrder = "N";
                                            } else {
                                                Debug.logInfo("Found item to pick: " + orderItemShipGrpInvResEntry, MODULE);
                                                if ((UtilValidate.isEmpty(orderItemShipGrpInvResEntry.get("quantityNotAvailable")) || java.util.Objects.equals(orderItemShipGrpInvResEntry.get("quantityNotAvailable"), BigDecimal.ZERO) || (orderItemShipGrpInvResEntry.get("quantity") != null /* TODO: field compare operator greater */ && "Y".equals(orderItemShipGroup.get("maySplit"))))) {
                                                    Debug.logInfo("Item has stock; flagging order (" + orderItemShipGrpInvResEntry.get("orderId") + ") as OK", MODULE);
                                                    hasStockToPick = "Y";
                                                } else {
                                                    Debug.logInfo("Item " + context.get("orderitemShipGrpInvRes") + " does not have stock and will not be flagged as hasStockToPick", MODULE);
                                                }
                                                try {
                                                    facilityLocation = inventoryItem.getRelatedOne("FacilityLocation", false);
                                                } catch (Exception e) {
                                                    Debug.logError(e, "Error getting related one FacilityLocation: " + e.getMessage(), MODULE);
                                                    return ServiceUtil.returnError(e.getMessage());
                                                }
                                                if (UtilValidate.isNotEmpty(facilityLocation)) {
                                                    if ("FLT_BULK".equals(facilityLocation.get("locationTypeEnumId"))) {
                                                        needsStockMove = "Y";
                                                    }
                                                }
                                                orderItemShipGrpInvResInfo.put("orderItemShipGrpInvRes", orderItemShipGrpInvResEntry);
                                                orderItemShipGrpInvResInfo.put("inventoryItem", inventoryItem);
                                                orderItemShipGrpInvResInfo.put("facilityLocation", facilityLocation);
                                                orderItemShipGrpInvResInfoList.add(orderItemShipGrpInvResInfo);
                                                orderItemShipGrpInvResInfo = new HashMap<String, Object>();
                                                finalOrderItemShipGrpInvResList.add(orderItemShipGrpInvResEntry);
                                            }
                                        }
                                    }
                                }
                            }
                            if ("N".equals(hasStockToPick)) {
                                pickThisOrder = "N";
                            }
                            if ((!(UtilValidate.isEmpty(context.get("maxNumberOfOrders"))) && numberSoFar != null /* TODO: field compare operator greater-equals */)) {
                                Debug.logInfo("We have passed the max number of orders!", MODULE);
                                pickThisOrder = "N";
                            } else {
                                Debug.logInfo("We have not passed the max number of orders yet...", MODULE);
                            }
                            if (("Y".equals(pickThisOrder) && "N".equals(allPickStarted))) {
                                orderHeaderInfo.put("orderHeader", orderHeaderEntry);
                                orderHeaderInfo.put("orderItemShipGroup", orderItemShipGroup);
                                orderHeaderInfo.put("orderItemAndShipGroupAssocList", orderItemAndShipGroupAssocList);
                                orderHeaderInfo.put("orderItemShipGrpInvResList", finalOrderItemShipGrpInvResList);
                                orderHeaderInfo.put("orderItemShipGrpInvResInfoList", orderItemShipGrpInvResInfoList);
                                if (UtilValidate.isEmpty(((Map<String, Object>) pickMoveInfoMap).get(groupName))) {
                                    GenericValue pickMoveInfoMap_groupName__groupName__shipmentMethodType = null;
                                    try {
                                        pickMoveInfoMap_groupName__groupName__shipmentMethodType = orderItemShipGroup.getRelatedOne("ShipmentMethodType", false);
                                    } catch (Exception e) {
                                        Debug.logError(e, "Error getting related one ShipmentMethodType: " + e.getMessage(), MODULE);
                                        return ServiceUtil.returnError(e.getMessage());
                                    }
                                }
                                if ("Y".equals(needsStockMove)) {
                                    pickMoveInfoMap_groupName__orderNeedsStockMoveInfoList.add(orderHeaderInfo);
                                    if ("Y".equals(orderHeaderEntry.get("isRushOrder"))) {
                                        rushOrderInfo_orderNeedsStockMoveInfoList.add(orderHeaderInfo);
                                    }
                                } else {
                                    pickMoveInfoMap_groupName__orderReadyToPickInfoList.add(orderHeaderInfo);
                                    if ("Y".equals(orderHeaderEntry.get("isRushOrder"))) {
                                        rushOrderInfo_orderReadyToPickInfoList.add(orderHeaderInfo);
                                    }
                                }
                                orderHeaderInfo = new HashMap<String, Object>();
                                numberSoFar = (new BigDecimal(numberSoFar.toString())).longValue();
                                Debug.logInfo("Added order #" + orderHeaderEntry.get("orderId") + " to pick list [" + numberSoFar + " of " + context.get("maxNumberOfOrders") + "] - " + pickThisOrder + " / " + allPickStarted, MODULE);
                                pickMoveInfoMap.put((String) groupName, groupName);
                                pickMoveInfoMap.put((String) groupName, groupName1);
                                pickMoveInfoMap.put((String) groupName, groupName2);
                                pickMoveInfoMap.put((String) groupName, noOfOrderItems);
                            } else {
                                Debug.logInfo("Order #" + orderHeaderEntry.get("orderId") + " was not added to pick list [" + numberSoFar + " of " + context.get("maxNumberOfOrders") + "] - " + pickThisOrder + " / " + allPickStarted, MODULE);
                            }
                            orderItemAndShipGroupAssocList = null;
                            orderItemShipGrpInvResInfoList = null;
                            finalOrderItemShipGrpInvResList = null;
                        } else {
                            Debug.logInfo("Order is not a member of the requested shipment method: " + context.get("shipmentMethodTypeId"), MODULE);
                        }
                    }
                }
                if (!(groupNames != null /* TODO: field compare operator contains */)) {
                    groupNames.add(groupName);
                }
                orderHeaderInfo = new HashMap<String, Object>();
                orderItemShipGroupList = null;
                orderItemShipGrpInvResList = null;
                if ((!(UtilValidate.isEmpty(maxNumberOfOrders)) && numberSoFar != null /* TODO: field compare operator greater-equals */)) {
                    break;
                }
            }
        }
        if (groupNames != null) {
            for (Object groupNameEntry : groupNames) {
                if (UtilValidate.isNotEmpty(((Map<String, Object>) pickMoveInfoMap).get(groupNameEntry))) {
                    pickMoveInfoList.add(((Map<String, Object>) pickMoveInfoMap).get(groupNameEntry));
                }
            }
        }
        groupNames = null;
        result.put("pickMoveInfoList", pickMoveInfoList);
        result.put("rushOrderInfo", context.get("rushOrderInfo"));

        return result;
    }


    /**
     * assembleOrderHeaderInfoInline
     */
    public static Map<String, Object> assembleOrderHeaderInfoInline(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue inventoryItem = null;
        Object reservedQuantity = null;
        Object issuedQuantity = null;
        Map<String, Object> inventoryItemQuantities = null;
        List<Object> perItemResListValid = null;
        List<Object> orderItemInfoList = null;
        Map<String, Object> inventoryItemOrderItems = null;
        Map<String, Object> itemFilterMap = null;
        Map<String, Object> inventoryItems = null;
        List<Object> orderHeaderInfoList = null;
        List<Object> wrongQuantityReservedList = null;
        Object inventoryItemQuantities_inventoryItemId_ = null;
        Map<String, Object> wrongQuantityReserved = null;
        List<GenericValue> itemIssuances = null;
        Object reservedIssuedQuantity = null;
        Object inventoryItemOrderItemList = null;
        Map<String, Object> orderHeaderInfo = null;
        List<GenericValue> perItemResList = null;
        Object orderItemInfo = null;
        List<Object> insufficientQohList = null;
        Object insufficientQoh = null;
        if (context.get("pickMoveInfoList") != null) {
            for (Object pickMoveInfo : (List<?>) context.get("pickMoveInfoList")) {
                if (((Map<String, Object>) pickMoveInfo).get("orderReadyToPickInfoList") != null) {
                    for (Object orderReadyToPickInfo : (List<?>) ((Map<String, Object>) pickMoveInfo).get("orderReadyToPickInfoList")) {
                        if (((Map<String, Object>) orderReadyToPickInfo).get("orderItemAndShipGroupAssocList") != null) {
                            for (Object orderItemAndShipGroupAssoc : (List<?>) ((Map<String, Object>) orderReadyToPickInfo).get("orderItemAndShipGroupAssocList")) {
                                if ("ITEM_APPROVED".equals(((Map<String, Object>) orderItemAndShipGroupAssoc).get("statusId"))) {
                                    reservedQuantity = 0;
                                    itemFilterMap.put("orderItemSeqId", ((Map<String, Object>) orderItemAndShipGroupAssoc).get("orderItemSeqId"));
                                    perItemResList = EntityUtil.filterByAnd(UtilGenerics.cast(((Map<String, Object>) orderReadyToPickInfo).get("orderItemShipGrpInvResList")), itemFilterMap);
                                    if (perItemResList != null) {
                                        for (GenericValue orderItemShipGrpInvRes : perItemResList) {
                                            Object inventoryItemId = orderItemShipGrpInvRes.get("inventoryItemId");
                                            inventoryItem = (GenericValue) ((Map<String, Object>) inventoryItems).get(inventoryItemId);
                                            if (UtilValidate.isEmpty(inventoryItem)) {
                                                try {
                                                    inventoryItem = EntityQuery.use(delegator)
                                                            .from("InventoryItem")
                                                            .where(context)
                                                            .queryOne();
                                                } catch (Exception e) {
                                                    Debug.logError(e, "Error querying InventoryItem: " + e.getMessage(), MODULE);
                                                    return ServiceUtil.returnError(e.getMessage());
                                                }
                                                inventoryItems.put((String) inventoryItemId, inventoryItem);
                                            }
                                            if (java.util.Objects.equals(inventoryItem.get("facilityId"), context.get("facilityId"))) {
                                                perItemResListValid.add(orderItemShipGrpInvRes);
                                                inventoryItemOrderItemList = ((Map<String, Object>) inventoryItemOrderItems).get(inventoryItemId);
                                                ((List<Object>) inventoryItemOrderItemList).add(orderItemAndShipGroupAssoc);
                                                inventoryItemOrderItems.put((String) orderItemShipGrpInvRes.get("inventoryItemId"), inventoryItemOrderItemList);
                                                inventoryItemOrderItemList = null;
                                                if (UtilValidate.isNotEmpty(((Map<String, Object>) inventoryItemQuantities).get(inventoryItemId))) {
                                                    ((Map<String, Object>) inventoryItemQuantities).put("inventoryItemId", new BigDecimal(orderItemShipGrpInvRes.get("quantity").toString()));
                                                } else {
                                                    inventoryItemQuantities.put((String) inventoryItemId, orderItemShipGrpInvRes.get("quantity"));
                                                }
                                            }
                                            inventoryItem = null;
                                            reservedQuantity = new BigDecimal(orderItemShipGrpInvRes.get("quantity").toString());
                                        }
                                    }
                                    if (UtilValidate.isNotEmpty(perItemResListValid)) {
                                        orderItemInfo = new HashMap<String, Object>();
                                        ((Map<String, Object>) orderItemInfo).put("orderItemAndShipGroupAssoc", orderItemAndShipGroupAssoc);
                                        ((Map<String, Object>) orderItemInfo).put("orderItemShipGrpInvResList", perItemResListValid);
                                        GenericValue orderItemInfo_product = null;
                                        try {
                                            orderItemInfo_product = ((GenericValue) orderItemAndShipGroupAssoc).getRelatedOne("Product", true);
                                        } catch (Exception e) {
                                            Debug.logError(e, "Error getting related one Product: " + e.getMessage(), MODULE);
                                            return ServiceUtil.returnError(e.getMessage());
                                        }
                                        orderItemInfoList.add(orderItemInfo);
                                    }
                                    perItemResListValid = null;
                                    try {
                                        itemIssuances = ((GenericValue) orderItemAndShipGroupAssoc).getRelated("ItemIssuance", null, null, false);
                                    } catch (Exception e) {
                                        Debug.logError(e, "Error getting related ItemIssuance: " + e.getMessage(), MODULE);
                                        return ServiceUtil.returnError(e.getMessage());
                                    }
                                    issuedQuantity = 0;
                                    if (itemIssuances != null) {
                                        for (GenericValue itemIssuance : itemIssuances) {
                                            issuedQuantity = new BigDecimal(issuedQuantity.toString());
                                        }
                                    }
                                    reservedIssuedQuantity = new BigDecimal(reservedQuantity.toString());
                                    if (!java.util.Objects.equals(reservedIssuedQuantity, ((Map<String, Object>) orderItemAndShipGroupAssoc).get("quantity"))) {
                                        wrongQuantityReserved.put("orderItemAndShipGroupAssoc", orderItemAndShipGroupAssoc);
                                        wrongQuantityReserved.put("reservedQuantity", reservedQuantity);
                                        wrongQuantityReserved.put("issuedQuantity", issuedQuantity);
                                        wrongQuantityReserved.put("reservedIssuedQuantity", reservedIssuedQuantity);
                                        wrongQuantityReservedList.add(wrongQuantityReserved);
                                        wrongQuantityReserved = new HashMap<String, Object>();
                                    }
                                }
                            }
                        }
                        if (UtilValidate.isNotEmpty(orderItemInfoList)) {
                            orderHeaderInfo.put("orderHeader", ((Map<String, Object>) orderReadyToPickInfo).get("orderHeader"));
                            orderHeaderInfo.put("orderItemShipGroup", ((Map<String, Object>) orderReadyToPickInfo).get("orderItemShipGroup"));
                            orderHeaderInfo.put("orderItemInfoList", orderItemInfoList);
                            orderHeaderInfoList.add(orderHeaderInfo);
                        }
                        orderHeaderInfo = new HashMap<String, Object>();
                        orderItemInfoList = null;
                    }
                }
            }
        }
        for (Map.Entry<String, Object> entry : ((Map<String, Object>) inventoryItemQuantities).entrySet()) {
            String inventoryItemId = entry.getKey();
            Object quantityNeeded = entry.getValue();
            inventoryItem = (GenericValue) ((Map<String, Object>) inventoryItems).get(inventoryItemId);
            Object insufficientQoh_inventoryItem = null;
            Object insufficientQoh_quantityNeeded = null;
            Object insufficientQohList__ = null;
            if ((((Comparable) quantityNeeded).compareTo(BigDecimal.ONE) <= 0 && (("SERIALIZED_INV_ITEM".equals(inventoryItem.get("inventoryItemTypeId")) && ((Comparable) quantityNeeded).compareTo(BigDecimal.ONE) < 0) || ("NON_SERIAL_INV_ITEM".equals(inventoryItem.get("inventoryItemTypeId")) && (UtilValidate.isEmpty(inventoryItem.get("quantityOnHandTotal")) || quantityNeeded != null /* TODO: field compare operator greater */))))) {
                insufficientQoh = new HashMap<String, Object>();
                ((Map<String, Object>) insufficientQoh).put("inventoryItem", inventoryItem);
                ((Map<String, Object>) insufficientQoh).put("quantityNeeded", quantityNeeded);
                insufficientQohList.add(insufficientQoh);
            }
        }

        return result;
    }


    /**
     * Create Picklist From Orders
     */
    public static Map<String, Object> createPicklistFromOrders(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Object quantityToSubtract = null;
        Map<String, Object> createPicklistItemMap = null;
        Object binLocationNumber = null;
        Object quantityToPick = null;
        Object picklistId = null;
        Object createPicklistBinMap = null;
        Object itemsInBin = null;
        Map<String, Object> createPicklistMap = null;
        Object picklistBinId = null;
        GenericValue binToRemove = null;
        GenericValue inventoryItem = null;
        GenericValue pickMoveInfo = null;
        Object reservedQuantity = null;
        GenericValue orderItemAndShipGroupAssoc = null;
        Object issuedQuantity = null;
        Map<String, Object> inventoryItemQuantities = null;
        List<Object> perItemResListValid = null;
        List<Object> orderItemInfoList = null;
        Map<String, Object> inventoryItemOrderItems = null;
        Map<String, Object> itemFilterMap = null;
        Map<String, Object> inventoryItems = null;
        GenericValue itemIssuance = null;
        List<Object> orderHeaderInfoList = null;
        List<Object> insufficientQohList = null;
        List<Object> wrongQuantityReservedList = null;
        Object inventoryItemQuantities_inventoryItemId_ = null;
        Map<String, Object> wrongQuantityReserved = null;
        List<GenericValue> itemIssuances = null;
        Object reservedIssuedQuantity = null;
        Object inventoryItemOrderItemList = null;
        Map<String, Object> orderHeaderInfo = null;
        List<GenericValue> perItemResList = null;
        GenericValue orderReadyToPickInfo = null;
        Object insufficientQoh = null;
        Object inventoryItemId = null;
        Object orderItemInfo = null;
        GenericValue orderItemShipGrpInvRes = null;
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        Map<String, Object> findOrdersToPickMoveMap = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "findOrdersToPickMoveMap" for service "findOrdersToPickMove"
        findOrdersToPickMoveMap.putAll(UtilMisc.toMap(context));
        Object pickMoveInfoList = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("findOrdersToPickMove", findOrdersToPickMoveMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            pickMoveInfoList = serviceResult.get("pickMoveInfoList");
        } catch (Exception e) {
            Debug.logError(e, "Error calling findOrdersToPickMove: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        context.put("inventoryItem", inventoryItem);
        context.put("pickMoveInfo", pickMoveInfo);
        context.put("reservedQuantity", reservedQuantity);
        context.put("orderItemAndShipGroupAssoc", orderItemAndShipGroupAssoc);
        context.put("issuedQuantity", issuedQuantity);
        context.put("inventoryItemQuantities", inventoryItemQuantities);
        context.put("perItemResListValid", perItemResListValid);
        context.put("orderItemInfoList", orderItemInfoList);
        context.put("inventoryItemOrderItems", inventoryItemOrderItems);
        context.put("itemFilterMap", itemFilterMap);
        context.put("inventoryItems", inventoryItems);
        context.put("itemIssuance", itemIssuance);
        context.put("orderHeaderInfoList", orderHeaderInfoList);
        context.put("insufficientQohList", insufficientQohList);
        context.put("wrongQuantityReservedList", wrongQuantityReservedList);
        context.put("wrongQuantityReserved", wrongQuantityReserved);
        context.put("itemIssuances", itemIssuances);
        context.put("pickMoveInfoList", pickMoveInfoList);
        context.put("reservedIssuedQuantity", reservedIssuedQuantity);
        context.put("inventoryItemOrderItemList", inventoryItemOrderItemList);
        context.put("orderHeaderInfo", orderHeaderInfo);
        context.put("perItemResList", perItemResList);
        context.put("orderReadyToPickInfo", orderReadyToPickInfo);
        context.put("insufficientQoh", insufficientQoh);
        context.put("inventoryItemId", inventoryItemId);
        context.put("orderItemInfo", orderItemInfo);
        context.put("orderItemShipGrpInvRes", orderItemShipGrpInvRes);
        assembleOrderHeaderInfoInline(dctx, context);
        inventoryItemQuantities_inventoryItemId_ = context.get("inventoryItemQuantities_inventoryItemId_");
        if (UtilValidate.isNotEmpty(orderHeaderInfoList)) {
            createPicklistMap.put("facilityId", context.get("facilityId"));
            createPicklistMap.put("shipmentMethodTypeId", context.get("shipmentMethodTypeId"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createPicklist", createPicklistMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
                picklistId = serviceResult.get("picklistId");
            } catch (Exception e) {
                Debug.logError(e, "Error calling createPicklist: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            result.put("picklistId", picklistId);
            Debug.logInfo("Created Picklist with ID " + picklistId, MODULE);
            binLocationNumber = 1;
            if (orderHeaderInfoList != null) {
                for (Object orderHeaderInfo_iter : orderHeaderInfoList) {
                    orderHeaderInfo = (Map<String, Object>) orderHeaderInfo_iter;
                    picklistBinId = null;
                    createPicklistBinMap = new HashMap<String, Object>();
                    ((Map<String, Object>) createPicklistBinMap).put("picklistId", picklistId);
                    ((Map<String, Object>) createPicklistBinMap).put("binLocationNumber", binLocationNumber);
                    ((Map<String, Object>) createPicklistBinMap).put("primaryOrderId", ((Map<String, Object>) ((Map<String, Object>) orderHeaderInfo).get("orderItemShipGroup")).get("orderId"));
                    ((Map<String, Object>) createPicklistBinMap).put("primaryShipGroupSeqId", ((Map<String, Object>) ((Map<String, Object>) orderHeaderInfo).get("orderItemShipGroup")).get("shipGroupSeqId"));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("createPicklistBin", (Map<String, Object>) createPicklistBinMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                        }
                        picklistBinId = serviceResult.get("picklistBinId");
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling createPicklistBin: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    binLocationNumber = (new BigDecimal(binLocationNumber.toString())).longValue();
                    itemsInBin = 0L;
                    if (((Map<String, Object>) orderHeaderInfo).get("orderItemInfoList") != null) {
                        for (GenericValue orderItemInfo_iter : (List<GenericValue>) ((Map<String, Object>) orderHeaderInfo).get("orderItemInfoList")) {
                            orderItemInfo = (Object) orderItemInfo_iter;
                            if (((Map<String, Object>) orderItemInfo).get("orderItemShipGrpInvResList") != null) {
                                for (Object orderItemShipGrpInvRes_iter : (List<?>) ((Map<String, Object>) orderItemInfo).get("orderItemShipGrpInvResList")) {
                                    orderItemShipGrpInvRes = (GenericValue) orderItemShipGrpInvRes_iter;
                                    Debug.logInfo("Getting pick quantity : " + orderItemShipGrpInvRes.get("quantity") + " - " + orderItemShipGrpInvRes.get("quantityNotAvailable"), MODULE);
                                    quantityToPick = orderItemShipGrpInvRes.get("quantity");
                                    if ((!(UtilValidate.isEmpty(orderItemShipGrpInvRes.get("quantityNotAvailable"))) && ((Comparable) orderItemShipGrpInvRes.get("quantityNotAvailable")).compareTo(0L) > 0)) {
                                        quantityToSubtract = orderItemShipGrpInvRes.get("quantityNotAvailable");
                                        Debug.logInfo("Subtracting " + quantityToSubtract + " from " + quantityToPick, MODULE);
                                        quantityToPick = new BigDecimal(quantityToSubtract.toString());
                                    }
                                    Debug.logInfo("Order #" + orderItemShipGrpInvRes.get("orderId") + " / " + orderItemShipGrpInvRes.get("orderItemSeqId") + " - " + quantityToPick, MODULE);
                                    if (((Comparable) quantityToPick).compareTo(BigDecimal.ZERO) > 0) {
                                        createPicklistItemMap = new HashMap<String, Object>();
                                        createPicklistItemMap.put("picklistBinId", picklistBinId);
                                        createPicklistItemMap.put("itemStatusId", "PICKITEM_PENDING");
                                        // set-service-fields from "orderItemShipGrpInvRes" to "createPicklistItemMap" for service "createPicklistItem"
                                        createPicklistItemMap.putAll(UtilMisc.toMap(orderItemShipGrpInvRes));
                                        createPicklistItemMap.put("quantity", quantityToPick);
                                        try {
                                            Map<String, Object> serviceResult = dispatcher.runSync("createPicklistItem", createPicklistItemMap);
                                            if (ServiceUtil.isError(serviceResult)) {
                                                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                                            }
                                        } catch (Exception e) {
                                            Debug.logError(e, "Error calling createPicklistItem: " + e.getMessage(), MODULE);
                                            return ServiceUtil.returnError(e.getMessage());
                                        }
                                        itemsInBin = BigDecimal.ZERO;
                                    }
                                    quantityToPick = null;
                                }
                            }
                        }
                    }
                    if ("0".equals(itemsInBin)) {
                        try {
                            binToRemove = EntityQuery.use(delegator)
                                    .from("PicklistBin")
                                    .where(UtilMisc.toMap("picklistBinId", picklistBinId))
                                    .queryOne();
                        } catch (Exception e) {
                            Debug.logError(e, "Error querying PicklistBin: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                        try {
                            delegator.removeValue(binToRemove);
                        } catch (Exception e) {
                            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                    }
                }
            }
        } else {
            Debug.logInfo("Not Creating Picklist with ID, nothing to process.", MODULE);
            {
                String errorMsg = UtilProperties.getMessage("ProductUiLabels", "FacilityNoOrdersReadyToPick", locale);
                error_list.add(errorMsg);
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        }

        return result;
    }


    /**
     * Print pick sheets for orders
     */
    public static Map<String, Object> printPickSheets(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue pickMoveInfo = null;
        Object groupName = null;
        Object printGroupName = null;
        List<Object> toPrintList = null;
        Map<String, Object> orderHeaderMap = null;
        Long counter = null;
        if (UtilValidate.isEmpty(context.get("maxNumberOfOrdersToPrint"))) {
            {
                String errorMsg = UtilProperties.getMessage("ProductErrorUiLabels", "ProductNumberOfOrdersMustNotBeEmptyToPrintPickSheet", locale);
                error_list.add(errorMsg);
            }
        } else {
            if (((Comparable) context.get("maxNumberOfOrdersToPrint")).compareTo(0L) <= 0) {
                {
                    String errorMsg = UtilProperties.getMessage("ProductErrorUiLabels", "ProductNumberOfOrdersMustBeGreaterThenZeroToPrintPickSheet", locale);
                    error_list.add(errorMsg);
                }
            }
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        Map<String, Object> findOrdersToPickMoveMap = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "findOrdersToPickMoveMap" for service "findOrdersToPickMove"
        findOrdersToPickMoveMap.putAll(UtilMisc.toMap(context));
        Object pickMoveInfoList = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("findOrdersToPickMove", findOrdersToPickMoveMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            pickMoveInfoList = serviceResult.get("pickMoveInfoList");
        } catch (Exception e) {
            Debug.logError(e, "Error calling findOrdersToPickMove: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(context.get("printGroupName"))) {
            printGroupName = context.get("printGroupName");
            if (pickMoveInfoList != null) {
                for (Object pickMoveInfo_iter : (List<?>) pickMoveInfoList) {
                    pickMoveInfo = (GenericValue) pickMoveInfo_iter;
                    groupName = pickMoveInfo.get("groupName");
                    if (java.util.Objects.equals(groupName, printGroupName)) {
                        toPrintList.addAll(UtilGenerics.cast(pickMoveInfo.get("orderReadyToPickInfoList")));
                    }
                }
            }
        } else {
            if (pickMoveInfoList != null) {
                for (Object pickMoveInfo_iter : (List<?>) pickMoveInfoList) {
                    pickMoveInfo = (GenericValue) pickMoveInfo_iter;
                    toPrintList.addAll(UtilGenerics.cast(pickMoveInfo.get("orderReadyToPickInfoList")));
                }
            }
        }
        counter = 0L;
        if (toPrintList != null) {
            for (Object toPrint : toPrintList) {
                if (counter != null /* TODO: field compare operator less */) {
                    orderHeaderMap.put("orderId", ((Map<String, Object>) ((Map<String, Object>) toPrint).get("orderHeader")).get("orderId"));
                    orderHeaderMap.put("pickSheetPrintedDate", nowTimestamp);
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("updateOrderHeader", orderHeaderMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling updateOrderHeader: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    counter = counter + 1L;
                    orderHeaderMap = new HashMap<String, Object>();
                }
            }
        }
        result.put("pickMoveInfoList", pickMoveInfoList);

        return result;
    }


    /**
     * Create Picklist
     */
    public static Map<String, Object> createPicklist(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue newEntity = null;
        newEntity = delegator.makeValue("Picklist");
        newEntity.setNonPKFields(context);
        ((GenericValue) newEntity).put("picklistId", delegator.getNextSeqId("Picklist"));
        result.put("picklistId", newEntity.get("picklistId"));
        if (UtilValidate.isEmpty(newEntity.get("statusId"))) {
            newEntity.put("statusId", "PICKLIST_INPUT");
        }
        Timestamp newEntity_picklistDate = new Timestamp(System.currentTimeMillis());
        newEntity.put("createdByUserLogin", userLogin.get("userLoginId"));
        newEntity.put("lastModifiedByUserLogin", userLogin.get("userLoginId"));
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Update Picklist
     */
    public static Map<String, Object> updatePicklist(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue newStatusValue = null;
        GenericValue checkStatusValidChange = null;
        GenericValue lookupPKMap = delegator.makeValue("Picklist");
        lookupPKMap.setPKFields(context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from(lookupPKMap.getEntityName())
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key : " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(context.get("statusId"))) {
            if (!java.util.Objects.equals(context.get("statusId"), lookedUpValue.get("statusId"))) {
                try {
                    checkStatusValidChange = EntityQuery.use(delegator)
                            .from("StatusValidChange")
                            .where(UtilMisc.toMap("statusId", lookedUpValue.get("statusId"), "statusIdTo", context.get("statusId")))
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying StatusValidChange: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (UtilValidate.isEmpty(checkStatusValidChange)) {
                    error_list.add("ERROR: Changing the status from " + lookedUpValue.get("statusId") + " to " + context.get("statusId") + " is not allowed.");
                }
                if (!error_list.isEmpty()) {
                    return ServiceUtil.returnError(error_list);
                }
                newStatusValue = delegator.makeValue("PicklistStatusHistory");
                newStatusValue.put("picklistId", context.get("picklistId"));
                newStatusValue.put("statusId", lookedUpValue.get("statusId"));
                newStatusValue.put("statusIdTo", context.get("statusId"));
                Timestamp newStatusValue_changeDate = new Timestamp(System.currentTimeMillis());
                newStatusValue.put("changeUserLoginId", userLogin.get("userLoginId"));
                try {
                    delegator.create(newStatusValue);
                } catch (Exception e) {
                    Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        result.put("oldStatusId", lookedUpValue.get("statusId"));
        lookedUpValue.setNonPKFields(context);
        lookedUpValue.put("lastModifiedByUserLogin", userLogin.get("userLoginId"));
        try {
            delegator.store(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Delete Picklist
     */
    public static Map<String, Object> deletePicklist(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookupPKMap = delegator.makeValue("Picklist");
        lookupPKMap.setPKFields(context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from(lookupPKMap.getEntityName())
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key : " + e.getMessage(), MODULE);
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
     * Create PicklistBin
     */
    public static Map<String, Object> createPicklistBin(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue newEntity = delegator.makeValue("PicklistBin");
        newEntity.setNonPKFields(context);
        ((GenericValue) newEntity).put("picklistBinId", delegator.getNextSeqId("PicklistBin"));
        result.put("picklistBinId", newEntity.get("picklistBinId"));
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Update PicklistBin
     */
    public static Map<String, Object> updatePicklistBin(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookupPKMap = delegator.makeValue("PicklistBin");
        lookupPKMap.setPKFields(context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from(lookupPKMap.getEntityName())
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key : " + e.getMessage(), MODULE);
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
     * Delete PicklistBin
     */
    public static Map<String, Object> deletePicklistBin(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookupPKMap = delegator.makeValue("PicklistBin");
        lookupPKMap.setPKFields(context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from(lookupPKMap.getEntityName())
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key : " + e.getMessage(), MODULE);
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
     * Create PicklistItem
     */
    public static Map<String, Object> createPicklistItem(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue newEntity = null;
        newEntity = delegator.makeValue("PicklistItem");
        newEntity.setPKFields(context);
        newEntity.setNonPKFields(context);
        if (UtilValidate.isEmpty(newEntity.get("itemStatusId"))) {
            newEntity.put("itemStatusId", "PICKITEM_PENDING");
        }
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Update PicklistItem
     */
    public static Map<String, Object> updatePicklistItem(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue checkStatusValidChange = null;
        GenericValue lookupPKMap = delegator.makeValue("PicklistItem");
        lookupPKMap.setPKFields(context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from(lookupPKMap.getEntityName())
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key : " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(context.get("itemStatusId"))) {
            if (!java.util.Objects.equals(context.get("itemStatusId"), lookedUpValue.get("itemStatusId"))) {
                try {
                    checkStatusValidChange = EntityQuery.use(delegator)
                            .from("StatusValidChange")
                            .where(UtilMisc.toMap("statusId", lookedUpValue.get("itemStatusId"), "statusIdTo", context.get("itemStatusId")))
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying StatusValidChange: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (UtilValidate.isEmpty(checkStatusValidChange)) {
                    error_list.add("ERROR: Changing the status from " + lookedUpValue.get("itemStatusId") + " to " + context.get("itemStatusId") + " is not allowed.");
                }
            }
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        result.put("oldItemStatusId", lookedUpValue.get("itemStatusId"));
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
     * Delete PicklistItem
     */
    public static Map<String, Object> deletePicklistItem(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookupPKMap = delegator.makeValue("PicklistItem");
        lookupPKMap.setPKFields(context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from(lookupPKMap.getEntityName())
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key : " + e.getMessage(), MODULE);
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
     * Edit a Picklist Item
     */
    public static Map<String, Object> editPicklistItem(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Object linkProductAndLot = null;
        Object quantity = null;
        Object inputMap = null;
        Object currentPli = null;
        Object actionOnPli = null;
        GenericValue lookupPKMap = delegator.makeValue("PicklistItem");
        lookupPKMap.setPKFields(context);
        GenericValue picklistItem = null;
        try {
            picklistItem = EntityQuery.use(delegator)
                    .from(lookupPKMap.getEntityName())
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key : " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (context.get("quantity") != null /* TODO: field compare operator greater */) {
            {
                String errorMsg = UtilProperties.getMessage("ProductErrorUiLabels", "PicklistManageTooMuchQuantity", locale);
                error_list.add(errorMsg);
            }
        }
        List<GenericValue> inventoryItemList = null;
        try {
            inventoryItemList = EntityQuery.use(delegator)
                    .from("InventoryItem")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying InventoryItem: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        linkProductAndLot = "ko";
        if (inventoryItemList != null) {
            for (GenericValue inventoryItem : inventoryItemList) {
                if (java.util.Objects.equals(context.get("productId"), inventoryItem.get("productId"))) {
                    linkProductAndLot = "ok";
                }
            }
        }
        if ("ko".equals(linkProductAndLot)) {
            {
                String errorMsg = UtilProperties.getMessage("ProductErrorUiLabels", "PicklistManageNoLinkProductAndLot", locale);
                error_list.add(errorMsg);
            }
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        ((Map<String, Object>) inputMap).put("productId", context.get("productId"));
        ((Map<String, Object>) inputMap).put("facilityId", context.get("facilityId"));
        ((Map<String, Object>) inputMap).put("lotId", context.get("lotId"));
        Object quantityOnHandTotal = null;
        Object availableToPromiseTotal = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("getInventoryAvailableByFacility", (Map<String, Object>) inputMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            quantityOnHandTotal = serviceResult.get("quantityOnHandTotal");
            availableToPromiseTotal = serviceResult.get("availableToPromiseTotal");
        } catch (Exception e) {
            Debug.logError(e, "Error calling getInventoryAvailableByFacility: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (context.get("quantity") != null /* TODO: field compare operator greater */) {
            {
                String errorMsg = UtilProperties.getMessage("ProductErrorUiLabels", "PicklistManageStockLow", locale);
                error_list.add(errorMsg);
            }
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue oisgir = null;
        try {
            oisgir = EntityQuery.use(delegator)
                    .from("OrderItemShipGrpInvRes")
                    .where(UtilMisc.toMap("orderId", context.get("orderId"), "shipGroupSeqId", context.get("shipGroupSeqId"), "orderItemSeqId", context.get("orderItemSeqId"), "inventoryItemId", context.get("inventoryItemId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderItemShipGrpInvRes: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            delegator.removeValue(picklistItem);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> cancelOrderItemShipGrpInvResMap = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "cancelOrderItemShipGrpInvResMap" for service "cancelOrderItemShipGrpInvRes"
        cancelOrderItemShipGrpInvResMap.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("cancelOrderItemShipGrpInvRes", cancelOrderItemShipGrpInvResMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling cancelOrderItemShipGrpInvRes: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        inputMap = new HashMap<String, Object>();
        ((Map<String, Object>) inputMap).put("orderId", context.get("orderId"));
        ((Map<String, Object>) inputMap).put("shipGroupSeqId", context.get("shipGroupSeqId"));
        ((Map<String, Object>) inputMap).put("orderItemSeqId", context.get("orderItemSeqId"));
        ((Map<String, Object>) inputMap).put("productId", context.get("productId"));
        ((Map<String, Object>) inputMap).put("quantity", context.get("quantity"));
        ((Map<String, Object>) inputMap).put("facilityId", context.get("facilityId"));
        ((Map<String, Object>) inputMap).put("requireInventory", "Y");
        ((Map<String, Object>) inputMap).put("reserveOrderEnumId", oisgir.get("reserveOrderEnumId"));
        ((Map<String, Object>) inputMap).put("lotId", context.get("lotId"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("reserveProductInventoryByFacility", (Map<String, Object>) inputMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling reserveProductInventoryByFacility: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (context.get("quantity") != null /* TODO: field compare operator less */) {
            inputMap = new HashMap<String, Object>();
            ((Map<String, Object>) inputMap).put("orderId", context.get("orderId"));
            ((Map<String, Object>) inputMap).put("shipGroupSeqId", context.get("shipGroupSeqId"));
            ((Map<String, Object>) inputMap).put("orderItemSeqId", context.get("orderItemSeqId"));
            ((Map<String, Object>) inputMap).put("productId", context.get("productId"));
            ((Map<String, Object>) inputMap).put("facilityId", context.get("facilityId"));
            if (UtilValidate.isNotEmpty(context.get("oldLotId"))) {
                ((Map<String, Object>) inputMap).put("lotId", context.get("oldLotId"));
            }
            quantity = new BigDecimal(context.get("quantity").toString());
            ((Map<String, Object>) inputMap).put("quantity", quantity);
            ((Map<String, Object>) inputMap).put("requireInventory", "Y");
            ((Map<String, Object>) inputMap).put("reserveOrderEnumId", oisgir.get("reserveOrderEnumId"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("reserveProductInventoryByFacility", (Map<String, Object>) inputMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling reserveProductInventoryByFacility: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        List<GenericValue> oisgirs = null;
        try {
            oisgirs = EntityQuery.use(delegator)
                    .from("OrderItemShipGrpInvRes")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderItemShipGrpInvRes: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Object picklistBinId = picklistItem.get("picklistBinId");
        List<GenericValue> picklistItemList = null;
        try {
            picklistItemList = EntityQuery.use(delegator)
                    .from("PicklistItem")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PicklistItem: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        inputMap = null;
        if (oisgirs != null) {
            for (GenericValue oisgirEntry : oisgirs) {
                actionOnPli = "new";
                if (UtilValidate.isNotEmpty(oisgirEntry.get("quantityNotAvailable"))) {
                    quantity = new BigDecimal(oisgirEntry.get("quantityNotAvailable").toString());
                } else {
                    quantity = oisgirEntry.get("quantity");
                }
                if (picklistItemList != null) {
                    for (GenericValue pli : picklistItemList) {
                        if (java.util.Objects.equals(pli.get("inventoryItemId"), oisgirEntry.get("inventoryItemId"))) {
                            if (java.util.Objects.equals(pli.get("quantity"), quantity)) {
                                actionOnPli = "none";
                            } else {
                                actionOnPli = "edit";
                                currentPli = "pli";
                            }
                        }
                    }
                }
                if ("new".equals(actionOnPli)) {
                    ((Map<String, Object>) inputMap).put("inventoryItemId", oisgirEntry.get("inventoryItemId"));
                    ((Map<String, Object>) inputMap).put("orderId", oisgirEntry.get("orderId"));
                    ((Map<String, Object>) inputMap).put("orderItemSeqId", oisgirEntry.get("orderItemSeqId"));
                    ((Map<String, Object>) inputMap).put("picklistBinId", picklistBinId);
                    ((Map<String, Object>) inputMap).put("quantity", quantity);
                    ((Map<String, Object>) inputMap).put("shipGroupSeqId", oisgirEntry.get("shipGroupSeqId"));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("createPicklistItem", (Map<String, Object>) inputMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling createPicklistItem: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
                if ("edit".equals(actionOnPli)) {
                    ((Map<String, Object>) inputMap).put("inventoryItemId", oisgirEntry.get("inventoryItemId"));
                    ((Map<String, Object>) inputMap).put("orderId", oisgirEntry.get("orderId"));
                    ((Map<String, Object>) inputMap).put("orderItemSeqId", oisgirEntry.get("orderItemSeqId"));
                    ((Map<String, Object>) inputMap).put("picklistBinId", picklistBinId);
                    ((Map<String, Object>) inputMap).put("quantity", quantity);
                    ((Map<String, Object>) inputMap).put("shipGroupSeqId", oisgirEntry.get("shipGroupSeqId"));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("updatePicklistItem", (Map<String, Object>) inputMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling updatePicklistItem: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            }
        }

        return result;
    }


    /**
     * Set the status of a pick list item to completed
     */
    public static Map<String, Object> setPicklistItemToComplete(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> serviceCtx = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "serviceCtx" for service "updatePicklistItem"
        serviceCtx.putAll(UtilMisc.toMap(context));
        serviceCtx.put("itemStatusId", "PICKITEM_COMPLETED");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updatePicklistItem", serviceCtx);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updatePicklistItem: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Create PicklistRole
     */
    public static Map<String, Object> createPicklistRole(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue newEntity = delegator.makeValue("PicklistRole");
        newEntity.setPKFields(context);
        newEntity.setNonPKFields(context);
        if (UtilValidate.isEmpty(newEntity.get("fromDate"))) {
            Timestamp newEntity_fromDate = new Timestamp(System.currentTimeMillis());
        }
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Update PicklistRole
     */
    public static Map<String, Object> updatePicklistRole(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookupPKMap = delegator.makeValue("PicklistRole");
        lookupPKMap.setPKFields(context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from(lookupPKMap.getEntityName())
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key : " + e.getMessage(), MODULE);
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
     * Delete PicklistRole
     */
    public static Map<String, Object> deletePicklistRole(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookupPKMap = delegator.makeValue("PicklistRole");
        lookupPKMap.setPKFields(context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from(lookupPKMap.getEntityName())
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key : " + e.getMessage(), MODULE);
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
     * Get Picklist Display Info
     */
    public static Map<String, Object> getPicklistDisplayInfo(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<Object> picklistInfoList = null;
        Map<String, Object> inlineResult = null;
        Integer highIndex = null;
        List<Object> picklistRoleInfoList = null;
        List<Object> picklistItemInfoList = null;
        GenericValue picklistItem = null;
        List<GenericValue> picklistStatusHistoryList = null;
        GenericValue picklistStatusHistory = null;
        Object picklistBinOrderList = null;
        List orderBy = null;
        Object picklistRoleInfo = null;
        Object picklistBinInfo = null;
        List<GenericValue> picklistItemList = null;
        GenericValue picklistRole = null;
        List<Object> picklistBinInfoList = null;
        GenericValue picklistBin = null;
        Object picklistStatusHistoryInfo = null;
        List<GenericValue> picklistRoleList = null;
        List<GenericValue> picklistBinList = null;
        Object picklistInfo = null;
        List<Object> picklistStatusHistoryInfoList = null;
        Object picklistItemInfo = null;
        Integer viewSize = (Integer) context.get("viewSize");
        Integer viewIndex = (Integer) context.get("viewIndex");
        List<GenericValue> picklistList = null;
        try {
            picklistList = EntityQuery.use(delegator)
                    .from("Picklist")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Picklist: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Long picklistCount = null;
        try {
            picklistCount = EntityQuery.use(delegator)
                    .from("Picklist")
                    .queryCount();
        } catch (Exception e) {
            Debug.logError(e, "Error counting Picklist: " + e.getMessage(), MODULE);
        }
        if (picklistList != null) {
            for (GenericValue picklist : picklistList) {
                context.put("picklistRoleInfoList", picklistRoleInfoList);
                context.put("picklistItemInfoList", picklistItemInfoList);
                context.put("picklistItem", picklistItem);
                context.put("picklistStatusHistoryList", picklistStatusHistoryList);
                context.put("picklistStatusHistory", picklistStatusHistory);
                context.put("picklistBinOrderList", picklistBinOrderList);
                context.put("picklist", picklist);
                context.put("orderBy", orderBy);
                context.put("picklistRoleInfo", picklistRoleInfo);
                context.put("picklistBinInfo", picklistBinInfo);
                context.put("picklistItemList", picklistItemList);
                context.put("picklistRole", picklistRole);
                context.put("picklistBinInfoList", picklistBinInfoList);
                context.put("picklistBin", picklistBin);
                context.put("picklistStatusHistoryInfo", picklistStatusHistoryInfo);
                context.put("picklistRoleList", picklistRoleList);
                context.put("picklistBinList", picklistBinList);
                context.put("picklistInfo", picklistInfo);
                context.put("picklistStatusHistoryInfoList", picklistStatusHistoryInfoList);
                context.put("picklistItemInfo", picklistItemInfo);
                getPicklistSingleInfoInline(dctx, context);
                picklistInfoList.add(picklistInfo);
            }
        }
        Integer lowIndex = ((Number) context.get("(viewIndex * viewSize)")).intValue() + 1;
        highIndex = ((Number) context.get("(viewIndex")).intValue() + ((Number) context.get("1) * viewSize")).intValue();
        if (highIndex != null /* TODO: field compare operator greater */) {
            highIndex = ((Number) picklistCount).intValue();
        }
        if (viewSize != null /* TODO: field compare operator greater */) {
            highIndex = ((Number) picklistCount).intValue();
        }
        result.put("picklistInfoList", picklistInfoList);
        result.put("viewIndex", viewIndex);
        result.put("viewSize", viewSize);
        result.put("lowIndex", lowIndex);
        result.put("highIndex", highIndex);
        result.put("picklistCount", picklistCount);

        return result;
    }


    /**
     * getPickAndPackReportInfo
     */
    public static Map<String, Object> getPickAndPackReportInfo(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue productValueTemp = null;
        Map<String, Object> picklistItemInfoListByProductIdMap = null;
        Object picklistItemInfoTempList = null;
        Map<String, Object> picklistItemInfoListByLocationSeqIdMap = null;
        Map<String, Object> picklistBinByIdMap = null;
        Map<String, Object> facilityLocationByLocationSeqIdMap = null;
        Map<String, Object> productByProductIdMap = null;
        GenericValue picklistBinInfo = null;
        List<Object> facilityLocationInfoList = null;
        Object facilityLocationInfo = null;
        List<Object> noLocationProductInfoList = null;
        Object productInfo = null;
        List<Object> picklistRoleInfoList = null;
        List<Object> picklistItemInfoList = null;
        GenericValue picklistItem = null;
        List<GenericValue> picklistStatusHistoryList = null;
        GenericValue picklistStatusHistory = null;
        Object picklistBinOrderList = null;
        List orderBy = null;
        Object picklistRoleInfo = null;
        List<GenericValue> picklistItemList = null;
        GenericValue picklistRole = null;
        List<Object> picklistBinInfoList = null;
        GenericValue picklistBin = null;
        Object picklistStatusHistoryInfo = null;
        List<GenericValue> picklistRoleList = null;
        List<GenericValue> picklistBinList = null;
        Object picklistInfo = null;
        List<Object> picklistStatusHistoryInfoList = null;
        Object picklistItemInfo = null;
        GenericValue picklist = null;
        try {
            picklist = EntityQuery.use(delegator)
                    .from("Picklist")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Picklist: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        context.put("picklistRoleInfoList", picklistRoleInfoList);
        context.put("picklistItemInfoList", picklistItemInfoList);
        context.put("picklistItem", picklistItem);
        context.put("picklistStatusHistoryList", picklistStatusHistoryList);
        context.put("picklistStatusHistory", picklistStatusHistory);
        context.put("picklistBinOrderList", picklistBinOrderList);
        context.put("picklist", picklist);
        context.put("orderBy", orderBy);
        context.put("picklistRoleInfo", picklistRoleInfo);
        context.put("picklistBinInfo", picklistBinInfo);
        context.put("picklistItemList", picklistItemList);
        context.put("picklistRole", picklistRole);
        context.put("picklistBinInfoList", picklistBinInfoList);
        context.put("picklistBin", picklistBin);
        context.put("picklistStatusHistoryInfo", picklistStatusHistoryInfo);
        context.put("picklistRoleList", picklistRoleList);
        context.put("picklistBinList", picklistBinList);
        context.put("picklistInfo", picklistInfo);
        context.put("picklistStatusHistoryInfoList", picklistStatusHistoryInfoList);
        context.put("picklistItemInfo", picklistItemInfo);
        getPicklistSingleInfoInline(dctx, context);
        result.put("picklistInfo", picklistInfo);
        if (((Map<String, Object>) picklistInfo).get("picklistBinInfoList") != null) {
            for (Object picklistBinInfo_iter : (List<?>) ((Map<String, Object>) picklistInfo).get("picklistBinInfoList")) {
                picklistBinInfo = (GenericValue) picklistBinInfo_iter;
                picklistBinByIdMap.put((String) ((GenericValue) picklistBinInfo.get("picklistBin")).getString("picklistBinId"), picklistBinInfo.get("picklistBin"));
                if (picklistBinInfo.get("picklistItemInfoList") != null) {
                    for (GenericValue picklistItemInfo_iter : (List<GenericValue>) picklistBinInfo.get("picklistItemInfoList")) {
                        picklistItemInfo = (Object) picklistItemInfo_iter;
                        GenericValue facilityLocation = null;
                        Object productId = null;
                        if (UtilValidate.isNotEmpty(((Map<String, Object>) ((Map<String, Object>) picklistItemInfo).get("inventoryItemAndLocation")).get("locationSeqId"))) {
                            facilityLocation = delegator.makeValue("FacilityLocation");
                            facilityLocationByLocationSeqIdMap.put((String) facilityLocation.get("locationSeqId"), facilityLocation);
                            picklistItemInfoTempList = null;
                            picklistItemInfoTempList = ((Map<String, Object>) picklistItemInfoListByLocationSeqIdMap).get(facilityLocation.get("locationSeqId"));
                            ((List<Object>) picklistItemInfoTempList).add(picklistItemInfo);
                            picklistItemInfoListByLocationSeqIdMap.put((String) facilityLocation.get("locationSeqId"), picklistItemInfoTempList);
                        } else {
                            if (UtilValidate.isNotEmpty(((Map<String, Object>) ((Map<String, Object>) picklistItemInfo).get("orderItem")).get("productId"))) {
                                productValueTemp = null;
                                productId = ((Map<String, Object>) ((Map<String, Object>) picklistItemInfo).get("inventoryItemAndLocation")).get("productId");
                                try {
                                    productValueTemp = EntityQuery.use(delegator)
                                            .from("Product")
                                            .where(UtilMisc.toMap("productId", productId))
                                            .cache()
                                            .queryOne();
                                } catch (Exception e) {
                                    Debug.logError(e, "Error querying Product: " + e.getMessage(), MODULE);
                                    return ServiceUtil.returnError(e.getMessage());
                                }
                                productByProductIdMap.put((String) productId, productValueTemp);
                                picklistItemInfoTempList = null;
                                picklistItemInfoTempList = ((Map<String, Object>) picklistItemInfoListByProductIdMap).get(productId);
                                ((List<Object>) picklistItemInfoTempList).add(picklistItemInfo);
                                picklistItemInfoListByProductIdMap.put((String) productId, picklistItemInfoTempList);
                            } else {
                                Debug.logWarning("No productId and no FacilityLocation, not showing in Picklist for PicklistItem: " + ((Map<String, Object>) picklistItemInfo).get("picklistItem"), MODULE);
                            }
                        }
                    }
                }
            }
        }
        for (Map.Entry<String, Object> entry : ((Map<String, Object>) facilityLocationByLocationSeqIdMap).entrySet()) {
            String locationSeqId = entry.getKey();
            Object facilityLocationList__ = entry.getValue();
        }
        List<Object> facilityLocsOrdLst = new LinkedList<>();
        facilityLocsOrdLst.add("+areaId");
        facilityLocsOrdLst.add("+aisleId");
        facilityLocsOrdLst.add("+sectionId");
        facilityLocsOrdLst.add("+levelId");
        facilityLocsOrdLst.add("+positionId");
        if (context.get("facilityLocationList") != null) {
            for (Object facilityLocationEntry : (List<?>) context.get("facilityLocationList")) {
                facilityLocationInfo = new HashMap<String, Object>();
                ((Map<String, Object>) facilityLocationInfo).put("facilityLocation", facilityLocationEntry);
                ((Map<String, Object>) facilityLocationInfo).put("picklistItemInfoList", ((Map<String, Object>) picklistItemInfoListByLocationSeqIdMap).get(((Map<String, Object>) facilityLocationEntry).get("locationSeqId")));
                if (((Map<String, Object>) facilityLocationInfo).get("picklistItemInfoList") != null) {
                    for (Object picklistItemInfo_iter : (List<?>) ((Map<String, Object>) facilityLocationInfo).get("picklistItemInfoList")) {
                        picklistItemInfo = picklistItemInfo_iter;
                        ((Map<String, Object>) facilityLocationInfo).put("pickQuantity", new BigDecimal(((GenericValue) ((Map<String, Object>) picklistItemInfo).get("picklistItem")).getString("quantity").toString()));
                        ((Map<String, Object>) facilityLocationInfo).put("picklistItemInfo.picklistBin.picklistBinId", new BigDecimal(((GenericValue) ((Map<String, Object>) picklistItemInfo).get("picklistItem")).getString("quantity").toString()));
                        if (UtilValidate.isEmpty(((Map<String, Object>) facilityLocationInfo).get("product"))) {
                            ((Map<String, Object>) facilityLocationInfo).put("product", ((Map<String, Object>) picklistItemInfo).get("product"));
                        } else {
                            if (!java.util.Objects.equals(((Map<String, Object>) ((Map<String, Object>) facilityLocationInfo).get("product")).get("productId"), ((Map<String, Object>) ((Map<String, Object>) picklistItemInfo).get("product")).get("productId"))) {
                                Debug.logError("When creating picklist report found in the same location [" + ((Map<String, Object>) facilityLocationEntry).get("locationSeqId") + "] two different products: " + ((Map<String, Object>) ((Map<String, Object>) facilityLocationInfo).get("product")).get("productId") + " and " + ((Map<String, Object>) ((Map<String, Object>) picklistItemInfo).get("product")).get("productId"), MODULE);
                                String facilityLocationInfo_message = "";
                            }
                        }
                    }
                }
                for (Map.Entry<String, Object> entry : ((Map<String, Object>) ((Map<String, Object>) facilityLocationInfo).get("quantityByPicklistBinIdMap")).entrySet()) {
                    String picklistBinId = entry.getKey();
                    Object quantity = entry.getValue();
                    picklistBinInfo = null;
                    picklistBinInfo.put("picklistBin", ((Map<String, Object>) picklistBinByIdMap).get(picklistBinId));
                    picklistBinInfo.put("quantity", quantity);
                    ((List<Object>) ((Map<String, Object>) facilityLocationInfo).get("picklistBinInfoList")).add(picklistBinInfo);
                }
                ((Map<String, Object>) facilityLocationInfo).put("picklistBinInfoList", EntityUtil.orderBy(UtilGenerics.cast(((Map<String, Object>) facilityLocationInfo).get("picklistBinInfoList")), UtilMisc.toList("picklistBin.binLocationNumber")));
                facilityLocationInfoList.add(facilityLocationInfo);
            }
        }
        for (Map.Entry<String, Object> entry : ((Map<String, Object>) productByProductIdMap).entrySet()) {
            String productId = entry.getKey();
            Object productList__ = entry.getValue();
        }
        List<Object> productsOrdLst = new LinkedList<>();
        productsOrdLst.add("+productId");
        if (context.get("productList") != null) {
            for (Object product : (List<?>) context.get("productList")) {
                productInfo = new HashMap<String, Object>();
                ((Map<String, Object>) productInfo).put("product", product);
                ((Map<String, Object>) productInfo).put("picklistItemInfoList", ((Map<String, Object>) picklistItemInfoListByProductIdMap).get(((Map<String, Object>) product).get("productId")));
                if (((Map<String, Object>) productInfo).get("picklistItemInfoList") != null) {
                    for (Object picklistItemInfo_iter : (List<?>) ((Map<String, Object>) productInfo).get("picklistItemInfoList")) {
                        picklistItemInfo = picklistItemInfo_iter;
                        ((Map<String, Object>) productInfo).put("pickQuantity", new BigDecimal(((Map<String, Object>) productInfo).get("pickQuantity").toString()));
                        ((Map<String, Object>) productInfo).put("picklistItemInfo.picklistBin.picklistBinId", new BigDecimal(((GenericValue) ((Map<String, Object>) picklistItemInfo).get("picklistItem")).getString("quantity").toString()));
                    }
                }
                for (Map.Entry<String, Object> entry : ((Map<String, Object>) ((Map<String, Object>) productInfo).get("quantityByPicklistBinIdMap")).entrySet()) {
                    String picklistBinId = entry.getKey();
                    Object quantity = entry.getValue();
                    picklistBinInfo = null;
                    picklistBinInfo.put("picklistBin", ((Map<String, Object>) picklistBinByIdMap).get(picklistBinId));
                    picklistBinInfo.put("quantity", quantity);
                    ((List<Object>) ((Map<String, Object>) productInfo).get("picklistBinInfoList")).add(picklistBinInfo);
                }
                ((Map<String, Object>) productInfo).put("picklistBinInfoList", EntityUtil.orderBy(UtilGenerics.cast(((Map<String, Object>) productInfo).get("picklistBinInfoList")), UtilMisc.toList("picklistBin.binLocationNumber")));
                noLocationProductInfoList.add(productInfo);
            }
        }
        result.put("facilityLocationInfoList", facilityLocationInfoList);
        result.put("noLocationProductInfoList", noLocationProductInfoList);

        return result;
    }


    /**
     * getPicklistSingleInfoInline
     */
    public static Map<String, Object> getPicklistSingleInfoInline(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<Object> picklistRoleInfoList = null;
        Object picklistRoleInfo = null;
        Object picklistStatusHistoryInfo = null;
        List<Object> picklistStatusHistoryInfoList = null;
        List<Object> picklistItemInfoList = null;
        Object picklistItemInfo = null;
        Object picklistBinInfo = null;
        List<GenericValue> picklistItemList = null;
        List<Object> picklistBinInfoList = null;
        picklistRoleInfoList = null;
        List<GenericValue> picklistRoleList = null;
        try {
            picklistRoleList = ((GenericValue) context.get("picklist")).getRelated("PicklistRole", null, null, false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related PicklistRole: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (picklistRoleList != null) {
            for (GenericValue picklistRole : picklistRoleList) {
                picklistRoleInfo = null;
                GenericValue picklistRoleInfo_partyNameView = null;
                try {
                    picklistRoleInfo_partyNameView = picklistRole.getRelatedOne("PartyNameView", true);
                } catch (Exception e) {
                    Debug.logError(e, "Error getting related one PartyNameView: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                GenericValue picklistRoleInfo_roleType = null;
                try {
                    picklistRoleInfo_roleType = picklistRole.getRelatedOne("RoleType", true);
                } catch (Exception e) {
                    Debug.logError(e, "Error getting related one RoleType: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                ((Map<String, Object>) picklistRoleInfo).put("picklistRole", picklistRole);
                ((List<Object>) picklistRoleInfoList).add(picklistRoleInfo);
            }
        }
        picklistStatusHistoryInfoList = null;
        List<GenericValue> picklistStatusHistoryList = null;
        try {
            picklistStatusHistoryList = ((GenericValue) context.get("picklist")).getRelated("PicklistStatusHistory", null, null, false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related PicklistStatusHistory: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (picklistStatusHistoryList != null) {
            for (GenericValue picklistStatusHistory : picklistStatusHistoryList) {
                picklistStatusHistoryInfo = null;
                GenericValue picklistStatusHistoryInfo_statusItem = null;
                try {
                    picklistStatusHistoryInfo_statusItem = picklistStatusHistory.getRelatedOne("StatusItem", true);
                } catch (Exception e) {
                    Debug.logError(e, "Error getting related one StatusItem: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                GenericValue picklistStatusHistoryInfo_statusItemTo = null;
                try {
                    picklistStatusHistoryInfo_statusItemTo = picklistStatusHistory.getRelatedOne("ToStatusItem", true);
                } catch (Exception e) {
                    Debug.logError(e, "Error getting related one ToStatusItem: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                ((Map<String, Object>) picklistStatusHistoryInfo).put("picklistStatusHistory", picklistStatusHistory);
                ((List<Object>) picklistStatusHistoryInfoList).add(picklistStatusHistoryInfo);
            }
        }
        Object picklistBinOrderList = null;
        picklistBinInfoList = null;
        ((List<Object>) picklistBinOrderList).add("+binLocationNumber");
        List<GenericValue> picklistBinList = null;
        try {
            picklistBinList = ((GenericValue) context.get("picklist")).getRelated("PicklistBin", null, null, false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related PicklistBin: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (picklistBinList != null) {
            for (GenericValue picklistBin : picklistBinList) {
                picklistBinInfo = null;
                GenericValue picklistBinInfo_primaryOrderHeader = null;
                try {
                    picklistBinInfo_primaryOrderHeader = picklistBin.getRelatedOne("PrimaryOrderHeader", false);
                } catch (Exception e) {
                    Debug.logError(e, "Error getting related one PrimaryOrderHeader: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                GenericValue picklistBinInfo_primaryOrderItemShipGroup = null;
                try {
                    picklistBinInfo_primaryOrderItemShipGroup = picklistBin.getRelatedOne("PrimaryOrderItemShipGroup", false);
                } catch (Exception e) {
                    Debug.logError(e, "Error getting related one PrimaryOrderItemShipGroup: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                GenericValue picklistBinInfo_productStore = null;
                try {
                    picklistBinInfo_productStore = ((GenericValue) ((Map<String, Object>) picklistBinInfo).get("primaryOrderHeader")).getRelatedOne("ProductStore", false);
                } catch (Exception e) {
                    Debug.logError(e, "Error getting related one ProductStore: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                picklistItemInfoList = null;
                try {
                    picklistItemList = picklistBin.getRelated("PicklistItem", null, null, true);
                } catch (Exception e) {
                    Debug.logError(e, "Error getting related PicklistItem: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (picklistItemList != null) {
                    for (GenericValue picklistItem : picklistItemList) {
                        picklistItemInfo = null;
                        GenericValue picklistItemInfo_orderItem = null;
                        try {
                            picklistItemInfo_orderItem = picklistItem.getRelatedOne("OrderItem", false);
                        } catch (Exception e) {
                            Debug.logError(e, "Error getting related one OrderItem: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                        GenericValue picklistItemInfo_product = null;
                        try {
                            picklistItemInfo_product = ((GenericValue) ((Map<String, Object>) picklistItemInfo).get("orderItem")).getRelatedOne("Product", false);
                        } catch (Exception e) {
                            Debug.logError(e, "Error getting related one Product: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                        GenericValue picklistItemInfo_inventoryItemAndLocation = null;
                        try {
                            picklistItemInfo_inventoryItemAndLocation = picklistItem.getRelatedOne("InventoryItemAndLocation", false);
                        } catch (Exception e) {
                            Debug.logError(e, "Error getting related one InventoryItemAndLocation: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                        GenericValue picklistItemInfo_orderItemShipGrpInvRes = null;
                        try {
                            picklistItemInfo_orderItemShipGrpInvRes = picklistItem.getRelatedOne("OrderItemShipGrpInvRes", false);
                        } catch (Exception e) {
                            Debug.logError(e, "Error getting related one OrderItemShipGrpInvRes: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                        List<GenericValue> picklistItemInfo_itemIssuanceList = null;
                        try {
                            picklistItemInfo_itemIssuanceList = picklistItem.getRelated("ItemIssuance", null, null, false);
                        } catch (Exception e) {
                            Debug.logError(e, "Error getting related ItemIssuance: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                        ((Map<String, Object>) picklistItemInfo).put("picklistItem", picklistItem);
                        ((Map<String, Object>) picklistItemInfo).put("picklistBin", picklistBin);
                        ((List<Object>) picklistItemInfoList).add(picklistItemInfo);
                    }
                }
                ((Map<String, Object>) picklistBinInfo).put("picklistItemInfoList", picklistItemInfoList);
                ((Map<String, Object>) picklistBinInfo).put("picklistBin", picklistBin);
                ((List<Object>) picklistBinInfoList).add(picklistBinInfo);
            }
        }
        Object picklistInfo = null;
        ((Map<String, Object>) picklistInfo).put("picklist", context.get("picklist"));
        ((Map<String, Object>) picklistInfo).put("picklistRoleInfoList", picklistRoleInfoList);
        ((Map<String, Object>) picklistInfo).put("picklistStatusHistoryInfoList", picklistStatusHistoryInfoList);
        ((Map<String, Object>) picklistInfo).put("picklistBinInfoList", picklistBinInfoList);
        List orderBy = UtilMisc.toList("sequenceId");
        GenericValue picklistInfo_statusItem = null;
        try {
            picklistInfo_statusItem = ((GenericValue) context.get("picklist")).getRelatedOne("StatusItem", true);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related one StatusItem: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue picklistInfo_facility = null;
        try {
            picklistInfo_facility = ((GenericValue) context.get("picklist")).getRelatedOne("Facility", true);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related one Facility: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue picklistInfo_shipmentMethodType = null;
        try {
            picklistInfo_shipmentMethodType = ((GenericValue) context.get("picklist")).getRelatedOne("ShipmentMethodType", true);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related one ShipmentMethodType: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        List<GenericValue> picklistInfo_statusValidChangeToDetailList = null;
        try {
            picklistInfo_statusValidChangeToDetailList = ((GenericValue) context.get("picklist")).getRelated("StatusValidChangeToDetail", null, null, false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related StatusValidChangeToDetail: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Get Picklist Data
     */
    public static Map<String, Object> getPicklistData(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Security security = dctx.getSecurity();
        List<String> error_list = new LinkedList<>();

        GenericValue inventoryItem = null;
        GenericValue pickMoveInfo = null;
        Object reservedQuantity = null;
        GenericValue orderItemAndShipGroupAssoc = null;
        Object issuedQuantity = null;
        Map<String, Object> inventoryItemQuantities = null;
        List<Object> perItemResListValid = null;
        List<Object> orderItemInfoList = null;
        Map<String, Object> inventoryItemOrderItems = null;
        Map<String, Object> itemFilterMap = null;
        Map<String, Object> inventoryItems = null;
        GenericValue itemIssuance = null;
        List<Object> orderHeaderInfoList = null;
        List<Object> insufficientQohList = null;
        List<Object> wrongQuantityReservedList = null;
        Object inventoryItemQuantities_inventoryItemId_ = null;
        Map<String, Object> wrongQuantityReserved = null;
        List<GenericValue> itemIssuances = null;
        Object reservedIssuedQuantity = null;
        Object inventoryItemOrderItemList = null;
        Map<String, Object> orderHeaderInfo = null;
        List<GenericValue> perItemResList = null;
        GenericValue orderReadyToPickInfo = null;
        Object insufficientQoh = null;
        Object inventoryItemId = null;
        Object orderItemInfo = null;
        GenericValue orderItemShipGrpInvRes = null;
        List<Object> facilityLocationInfoList = null;
        Object facilityLocationInfo = null;
        Map<String, Object> productInfoMap = null;
        GenericValue orderItem = null;
        Map<String, Object> facilityLocationMap = null;
        Map<String, Object> inventoryItemsByLocation = null;
        List<Object> facilityLocsOrdLst = null;
        List<Object> facilityLocations = null;
        Map<String, Object> orderItemMap = null;
        Object facilityLocation = null;
        Object inventoryItemIdList = null;
        List<Object> noLocationInventoryItemIds = null;
        List<Object> inventoryItemInfoList = null;
        Object inventoryItemInfo = null;
        if (!security.hasEntityPermission("FACILITY", "_VIEW", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("ProductUiLabels", "ProductFacilityViewPermissionError", locale));
        }
        if (!security.hasEntityPermission("FACILITY", "_UPDATE", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("ProductUiLabels", "ProductFacilityUpdatePermissionError", locale));
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        Map<String, Object> findOrdersToPickMoveMap = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "findOrdersToPickMoveMap" for service "findOrdersToPickMove"
        findOrdersToPickMoveMap.putAll(UtilMisc.toMap(context));
        Object pickMoveInfoList = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("findOrdersToPickMove", findOrdersToPickMoveMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            pickMoveInfoList = serviceResult.get("pickMoveInfoList");
        } catch (Exception e) {
            Debug.logError(e, "Error calling findOrdersToPickMove: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        context.put("inventoryItem", inventoryItem);
        context.put("pickMoveInfo", pickMoveInfo);
        context.put("reservedQuantity", reservedQuantity);
        context.put("orderItemAndShipGroupAssoc", orderItemAndShipGroupAssoc);
        context.put("issuedQuantity", issuedQuantity);
        context.put("inventoryItemQuantities", inventoryItemQuantities);
        context.put("perItemResListValid", perItemResListValid);
        context.put("orderItemInfoList", orderItemInfoList);
        context.put("inventoryItemOrderItems", inventoryItemOrderItems);
        context.put("itemFilterMap", itemFilterMap);
        context.put("inventoryItems", inventoryItems);
        context.put("itemIssuance", itemIssuance);
        context.put("orderHeaderInfoList", orderHeaderInfoList);
        context.put("insufficientQohList", insufficientQohList);
        context.put("wrongQuantityReservedList", wrongQuantityReservedList);
        context.put("wrongQuantityReserved", wrongQuantityReserved);
        context.put("itemIssuances", itemIssuances);
        context.put("pickMoveInfoList", pickMoveInfoList);
        context.put("reservedIssuedQuantity", reservedIssuedQuantity);
        context.put("inventoryItemOrderItemList", inventoryItemOrderItemList);
        context.put("orderHeaderInfo", orderHeaderInfo);
        context.put("perItemResList", perItemResList);
        context.put("orderReadyToPickInfo", orderReadyToPickInfo);
        context.put("insufficientQoh", insufficientQoh);
        context.put("inventoryItemId", inventoryItemId);
        context.put("orderItemInfo", orderItemInfo);
        context.put("orderItemShipGrpInvRes", orderItemShipGrpInvRes);
        assembleOrderHeaderInfoInline(dctx, context);
        inventoryItemQuantities_inventoryItemId_ = context.get("inventoryItemQuantities_inventoryItemId_");
        context.put("facilityLocationInfoList", facilityLocationInfoList);
        context.put("inventoryItem", inventoryItem);
        context.put("facilityLocationInfo", facilityLocationInfo);
        context.put("productInfoMap", productInfoMap);
        context.put("orderItem", orderItem);
        context.put("facilityLocationMap", facilityLocationMap);
        context.put("inventoryItemQuantities", inventoryItemQuantities);
        context.put("inventoryItemsByLocation", inventoryItemsByLocation);
        context.put("facilityLocsOrdLst", facilityLocsOrdLst);
        context.put("facilityLocations", facilityLocations);
        context.put("inventoryItemId", inventoryItemId);
        context.put("inventoryItemOrderItems", inventoryItemOrderItems);
        context.put("inventoryItems", inventoryItems);
        context.put("orderItemMap", orderItemMap);
        context.put("facilityLocation", facilityLocation);
        context.put("inventoryItemIdList", inventoryItemIdList);
        context.put("noLocationInventoryItemIds", noLocationInventoryItemIds);
        context.put("inventoryItemInfoList", inventoryItemInfoList);
        context.put("inventoryItemInfo", inventoryItemInfo);
        assembleFacilityLocationInfoInline(dctx, context);
        result.put("orderHeaderInfoList", orderHeaderInfoList);
        result.put("wrongQuantityReservedList", wrongQuantityReservedList);
        result.put("insufficientQohList", insufficientQohList);
        result.put("facilityLocationInfoList", facilityLocationInfoList);
        result.put("inventoryItemInfoList", inventoryItemInfoList);

        return result;
    }


    /**
     * assembleFacilityLocationInfoInline
     */
    public static Map<String, Object> assembleFacilityLocationInfoInline(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue inventoryItem = null;
        Object inventoryItemIdList = null;
        Map<String, Object> facilityLocationMap = null;
        List<Object> noLocationInventoryItemIds = null;
        Map<String, Object> inventoryItemsByLocation = null;
        List<Object> facilityLocations = null;
        List<Object> facilityLocationInfoList = null;
        Map<String, Object> orderItemMap = null;
        Object facilityLocationInfo = null;
        Map<String, Object> productInfoMap = null;
        List<Object> inventoryItemInfoList = null;
        Object inventoryItemInfo = null;
        for (Map.Entry<String, Object> entry : ((Map<String, Object>) context.get("inventoryItemQuantities")).entrySet()) {
            String inventoryItemId = entry.getKey();
            Object quantityNeeded = entry.getValue();
            inventoryItem = (GenericValue) ((Map<String, Object>) context.get("inventoryItems")).get(inventoryItemId);
            Object facilityLocation = null;
            try {
                facilityLocation = inventoryItem.getRelatedOne("FacilityLocation", false);
            } catch (Exception e) {
                Debug.logError(e, "Error getting related one FacilityLocation: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (UtilValidate.isNotEmpty(facilityLocation)) {
                facilityLocationMap.put((String) ((GenericValue) facilityLocation).get("locationSeqId"), facilityLocation);
                inventoryItemIdList = null;
                inventoryItemIdList = ((Map<String, Object>) inventoryItemsByLocation).get(((GenericValue) facilityLocation).get("locationSeqId"));
                ((List<Object>) inventoryItemIdList).add(inventoryItemId);
                inventoryItemsByLocation.put((String) ((GenericValue) facilityLocation).get("locationSeqId"), inventoryItemIdList);
            } else {
                noLocationInventoryItemIds.add(inventoryItemId);
            }
        }
        for (Map.Entry<String, Object> entry : ((Map<String, Object>) facilityLocationMap).entrySet()) {
            String locationSeqId = entry.getKey();
            Object facilityLocation = entry.getValue();
            facilityLocations.add(facilityLocation);
        }
        List<Object> facilityLocsOrdLst = new LinkedList<>();
        facilityLocsOrdLst.add("+areaId");
        facilityLocsOrdLst.add("+aisleId");
        facilityLocsOrdLst.add("+sectionId");
        facilityLocsOrdLst.add("+levelId");
        facilityLocsOrdLst.add("+positionId");
        if (facilityLocations != null) {
            for (Object facilityLocation : facilityLocations) {
                facilityLocationInfo = new HashMap<String, Object>();
                ((Map<String, Object>) facilityLocationInfo).put("facilityLocation", facilityLocation);
                inventoryItemIdList = ((Map<String, Object>) inventoryItemsByLocation).get(((Map<String, Object>) facilityLocation).get("locationSeqId"));
                if (inventoryItemIdList != null) {
                    for (Object inventoryItemId : (List<?>) inventoryItemIdList) {
                        inventoryItemInfo = new HashMap<String, Object>();
                        ((Map<String, Object>) inventoryItemInfo).put("facilityLocation", facilityLocation);
                        ((Map<String, Object>) inventoryItemInfo).put("inventoryItem", ((Map<String, Object>) context.get("inventoryItems")).get(inventoryItemId));
                        ((Map<String, Object>) inventoryItemInfo).put("orderItems", ((Map<String, Object>) context.get("inventoryItemOrderItems")).get(inventoryItemId));
                        ((Map<String, Object>) inventoryItemInfo).put("quantity", ((Map<String, Object>) context.get("inventoryItemQuantities")).get(inventoryItemId));
                        GenericValue inventoryItemInfo_product = null;
                        try {
                            inventoryItemInfo_product = ((GenericValue) ((Map<String, Object>) inventoryItemInfo).get("inventoryItem")).getRelatedOne("Product", false);
                        } catch (Exception e) {
                            Debug.logError(e, "Error getting related one Product: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                        GenericValue inventoryItemInfo_statusItem = null;
                        try {
                            inventoryItemInfo_statusItem = ((GenericValue) ((Map<String, Object>) inventoryItemInfo).get("inventoryItem")).getRelatedOne("StatusItem", false);
                        } catch (Exception e) {
                            Debug.logError(e, "Error getting related one StatusItem: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                        inventoryItemInfoList.add(inventoryItemInfo);
                        productInfoMap.put((String) ((Map<String, Object>) ((Map<String, Object>) context.get("${inventoryItemInfo")).get("product")).get("productId}.product"), ((Map<String, Object>) inventoryItemInfo).get("product"));
                        ((Map<String, Object>) productInfoMap).put("${inventoryItemInfo.product.productId}.quantity", new BigDecimal(((Map<String, Object>) ((Map<String, Object>) productInfoMap).get("${inventoryItemInfo")).get("product.productId}.quantity").toString()));
                        ((List<Object>) ((Map<String, Object>) ((Map<String, Object>) productInfoMap).get("${inventoryItemInfo")).get("product.productId}.inventoryItemList")).add(((Map<String, Object>) inventoryItemInfo).get("inventoryItem"));
                        if (((Map<String, Object>) inventoryItemInfo).get("orderItems") != null) {
                            for (Object orderItem : (List<?>) ((Map<String, Object>) inventoryItemInfo).get("orderItems")) {
                                orderItemMap.put(((GenericValue) orderItem).getString("orderId") + ":" + ((GenericValue) orderItem).getString("orderItemSeqId"), orderItem);
                            }
                        }
                    }
                }
                for (Map.Entry<String, Object> entry : ((Map<String, Object>) orderItemMap).entrySet()) {
                    String orderItemCompositeId = entry.getKey();
                    Object orderItem = entry.getValue();
                    ((List<Object>) ((Map<String, Object>) productInfoMap.get(((GenericValue) orderItem).getString("productId"))).get("orderItemList")).add(orderItem);
                }
                for (Map.Entry<String, Object> entry : ((Map<String, Object>) productInfoMap).entrySet()) {
                    String productId = entry.getKey();
                    Object productInfo = entry.getValue();
                    ((List<Object>) ((Map<String, Object>) facilityLocationInfo).get("productInfoList")).add(productInfo);
                }
                facilityLocationInfoList.add(facilityLocationInfo);
                orderItemMap = new HashMap<String, Object>();
                productInfoMap = new HashMap<String, Object>();
            }
        }
        if (noLocationInventoryItemIds != null) {
            for (Object inventoryItemIdEntry : noLocationInventoryItemIds) {
                ((Map<String, Object>) inventoryItemInfo).put("inventoryItem", ((Map<String, Object>) context.get("inventoryItems")).get(inventoryItemIdEntry));
                ((Map<String, Object>) inventoryItemInfo).put("orderItems", ((Map<String, Object>) context.get("inventoryItemOrderItems")).get(inventoryItemIdEntry));
                ((Map<String, Object>) inventoryItemInfo).put("quantity", ((Map<String, Object>) context.get("inventoryItemQuantities")).get(inventoryItemIdEntry));
                GenericValue inventoryItemInfo_product = null;
                try {
                    inventoryItemInfo_product = ((GenericValue) ((Map<String, Object>) inventoryItemInfo).get("inventoryItem")).getRelatedOne("Product", false);
                } catch (Exception e) {
                    Debug.logError(e, "Error getting related one Product: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                GenericValue inventoryItemInfo_statusItem = null;
                try {
                    inventoryItemInfo_statusItem = ((GenericValue) ((Map<String, Object>) inventoryItemInfo).get("inventoryItem")).getRelatedOne("StatusItem", false);
                } catch (Exception e) {
                    Debug.logError(e, "Error getting related one StatusItem: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                inventoryItemInfoList.add(inventoryItemInfo);
                inventoryItemInfo = null;
            }
        }

        return result;
    }


    /**
     * Checks the item status and updates the pick list status
     */
    public static Map<String, Object> checkPicklistBinItemStatuses(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Boolean allCancelled = null;
        Boolean allCompleteOrCancelled = null;
        GenericValue picklist = null;
        GenericValue binLookup = delegator.makeValue("PicklistBin");
        binLookup.setPKFields(context);
        GenericValue picklistBin = null;
        try {
            picklistBin = EntityQuery.use(delegator)
                    .from(binLookup.getEntityName())
                    .where(binLookup)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key : " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue plLookup = delegator.makeValue("Picklist");
        plLookup.setPKFields((Map<String, Object>) picklistBin);
        try {
            picklist = EntityQuery.use(delegator)
                    .from(plLookup.getEntityName())
                    .where(plLookup)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key : " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        List<GenericValue> picklistItem = null;
        try {
            picklistItem = EntityQuery.use(delegator)
                    .from("PicklistItemAndBin")
                    .where(UtilMisc.toMap("picklistId", picklist.get("picklistId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        allCompleteOrCancelled = Boolean.TRUE;
        allCancelled = Boolean.TRUE;
        if (picklistItem != null) {
            for (GenericValue item : picklistItem) {
                Debug.logInfo("checking status for item: " + item, MODULE);
                if (!"PICKITEM_CANCELLED".equals(item.get("itemStatusId"))) {
                    Debug.logInfo("item is not cancelled; all cancelled set to false", MODULE);
                    allCancelled = Boolean.FALSE;
                    if (!"PICKITEM_COMPLETED".equals(item.get("itemStatusId"))) {
                        Debug.logInfo("item is not completed; all completed set to false", MODULE);
                        allCompleteOrCancelled = Boolean.FALSE;
                    }
                }
            }
        }
        if (Boolean.TRUE.equals(allCancelled)) {
            Debug.logInfo("Setting picklist #" + picklist.get("picklistId") + " to cancelled", MODULE);
            picklist.put("statusId", "PICKLIST_CANCELLED");
            try {
                delegator.store(picklist);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        } else {
            Debug.logInfo("Not all items were cancelled; now check if we can complete the picklist : " + allCompleteOrCancelled, MODULE);
            if (Boolean.TRUE.equals(allCompleteOrCancelled)) {
                Debug.logInfo("Setting picklist #" + picklist.get("picklistId") + " to completed", MODULE);
                picklist.put("statusId", "PICKLIST_PICKED");
                try {
                    delegator.store(picklist);
                } catch (Exception e) {
                    Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }

        return result;
    }


    /**
     * If Picklist is Cancelled then cancel all the PicklistItem
     */
    public static Map<String, Object> cancelPicklistAndItems(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<GenericValue> itemList = null;
        List<GenericValue> picklistBinList = null;
        try {
            picklistBinList = EntityQuery.use(delegator)
                    .from("PicklistBin")
                    .where(UtilMisc.toMap("picklistId", context.get("picklistId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(picklistBinList)) {
            if (picklistBinList != null) {
                for (GenericValue picklistBin : picklistBinList) {
                    try {
                        itemList = EntityQuery.use(delegator)
                                .from("PicklistItem")
                                .where(UtilMisc.toMap("picklistBinId", picklistBin.get("picklistBinId")))
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    if (UtilValidate.isNotEmpty(itemList)) {
                        if (itemList != null) {
                            for (GenericValue item : itemList) {
                                item.put("itemStatusId", "PICKITEM_CANCELLED");
                                try {
                                    delegator.store(item);
                                } catch (Exception e) {
                                    Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                                    return ServiceUtil.returnError(e.getMessage());
                                }
                            }
                        }
                    }
                }
            }
        }

        return result;
    }

}

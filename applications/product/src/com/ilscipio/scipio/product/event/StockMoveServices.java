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
import org.ofbiz.security.Security;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://product/script/org/ofbiz/product/inventory/StockMoveServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class StockMoveServices {

    private static final String MODULE = StockMoveServices.class.getName();


    /**
     * Find all Stock Moves that need to be done
     */
    public static Map<String, Object> findStockMovesNeeded(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue orderItemShipGrpInvResAndItemLocation = null;
        List<Object> oiirailByLocMap_orderItemShipGrpInvResAndItemLocation_locationSeqId_ = null;
        List<Object> oiirailByProdMap_orderItemShipGrpInvResAndItemLocation_productId_ = null;
        List<GenericValue> targetInventoryItemList = null;
        GenericValue inventoryItem = null;
        Object totalQuantityOnHand = null;
        Map<String, Object> stockMoveHandled = null;
        GenericValue productFacilityLocationView = null;
        List<GenericValue> inventoryItemList = null;
        List<String> warningMessageList = null;
        Object targetTotalAvailableToPromise = null;
        List<Object> moveByOisgirInfoList = null;
        Object oiirailByProdMap = null;
        List<GenericValue> productFacilityLocationViewList = null;
        Object targetTotalQuantityOnHand = null;
        Object totalAvailableToPromise = null;
        Map<String, Object> moveInfo = null;
        List<GenericValue> orderItemShipGrpInvResAndItemLocationList = null;
        try {
            orderItemShipGrpInvResAndItemLocationList = EntityQuery.use(delegator)
                    .from("OrderItemShipGrpInvResAndItemLocation")
                    .where(UtilMisc.toMap("locationTypeEnumId", "FLT_BULK", "orderItemStatusId", "ITEM_APPROVED", "facilityId", context.get("facilityId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (orderItemShipGrpInvResAndItemLocationList != null) {
            for (GenericValue orderItemShipGrpInvResAndItemLocation_iter : orderItemShipGrpInvResAndItemLocationList) {
                orderItemShipGrpInvResAndItemLocation = orderItemShipGrpInvResAndItemLocation_iter;
                oiirailByLocMap_orderItemShipGrpInvResAndItemLocation_locationSeqId_.add(orderItemShipGrpInvResAndItemLocation);
            }
        }
        for (Map.Entry<String, Object> entry : ((Map<String, Object>) context.get("oiirailByLocMap")).entrySet()) {
            String locationSeqId = entry.getKey();
            Object perLocationOiirailList = entry.getValue();
            oiirailByProdMap = null;
            if (perLocationOiirailList != null) {
                for (Object orderItemShipGrpInvResAndItemLocation_iter : (List<?>) perLocationOiirailList) {
                    orderItemShipGrpInvResAndItemLocation = (GenericValue) orderItemShipGrpInvResAndItemLocation_iter;
                    oiirailByProdMap_orderItemShipGrpInvResAndItemLocation_productId_.add(orderItemShipGrpInvResAndItemLocation);
                }
            }
            Object perProductOiirailList = null;
            Object productId = null;
            for (Map.Entry<String, Object> entry2 : ((Map<String, Object>) oiirailByProdMap).entrySet()) {
                productId = entry2.getKey();
                perProductOiirailList = entry2.getValue();
                GenericValue moveInfo_product = null;
                try {
                    moveInfo_product = EntityQuery.use(delegator)
                            .from("Product")
                            .where(context)
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying Product: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                GenericValue moveInfo_facilityLocationFrom = null;
                try {
                    moveInfo_facilityLocationFrom = EntityQuery.use(delegator)
                            .from("FacilityLocation")
                            .where(UtilMisc.toMap("facilityId", context.get("facilityId"), "locationSeqId", locationSeqId))
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying FacilityLocation: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                try {
                    productFacilityLocationViewList = EntityQuery.use(delegator)
                            .from("ProductFacilityLocationView")
                            .where(UtilMisc.toMap("productId", productId, "facilityId", context.get("facilityId"), "locationTypeEnumId", "FLT_PICKLOC"))
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                GenericValue perProductOiirail = null;
                if (UtilValidate.isEmpty(productFacilityLocationViewList)) {
                    warningMessageList.add("Error in stock move, could not find a pick/primary location for facility [" + context.get("facilityId") + "] and product [" + productId + "]");
                } else {
                    productFacilityLocationView = EntityUtil.getFirst((List<GenericValue>) productFacilityLocationViewList);
                    GenericValue moveInfo_facilityLocationTo = null;
                    try {
                        moveInfo_facilityLocationTo = productFacilityLocationView.getRelatedOne("FacilityLocation", false);
                    } catch (Exception e) {
                        Debug.logError(e, "Error getting related one FacilityLocation: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    GenericValue moveInfo_targetProductFacilityLocation = null;
                    try {
                        moveInfo_targetProductFacilityLocation = productFacilityLocationView.getRelatedOne("ProductFacilityLocation", false);
                    } catch (Exception e) {
                        Debug.logError(e, "Error getting related one ProductFacilityLocation: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    ((Map<String, Object>) moveInfo).put("totalQuantity", 0);
                    if (perProductOiirailList != null) {
                        for (Object perProductOiirailEntry : (List<?>) perProductOiirailList) {
                            ((Map<String, Object>) moveInfo).put("totalQuantity", new BigDecimal(((Map<String, Object>) perProductOiirailEntry).get("quantity").toString()));
                        }
                    }
                    try {
                        inventoryItemList = EntityQuery.use(delegator)
                                .from("InventoryItem")
                                .where(UtilMisc.toMap("productId", productId, "facilityId", context.get("facilityId"), "locationSeqId", locationSeqId))
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    totalQuantityOnHand = 0;
                    totalAvailableToPromise = 0;
                    if (inventoryItemList != null) {
                        for (GenericValue inventoryItem_iter : inventoryItemList) {
                            inventoryItem = inventoryItem_iter;
                            totalQuantityOnHand = new BigDecimal(totalQuantityOnHand.toString());
                            totalAvailableToPromise = new BigDecimal(totalAvailableToPromise.toString());
                        }
                    }
                    moveInfo.put("quantityOnHandTotalFrom", totalQuantityOnHand);
                    moveInfo.put("availableToPromiseTotalFrom", totalAvailableToPromise);
                    if (totalQuantityOnHand != null /* TODO: field compare operator less */) {
                        warningMessageList.add("Warning in stock move: for facility [" + context.get("facilityId") + "] and product [" + productId + "] going from location [" + ((Map<String, Object>) context.get("productFacilityLocation")).get("locationSeqId") + "] to location [" + ((Map<String, Object>) ((Map<String, Object>) moveInfo).get("targetProductFacilityLocation")).get("locationSeqId") + "] a quantity of [" + ((Map<String, Object>) moveInfo).get("totalQuantity") + "] was needed but there are only [" + totalQuantityOnHand + "] on hand (this will be in the pick list with the full quantity on hand, but note that this will not be enough to prepare for all orders reserved against this location)");
                        moveInfo.put("totalQuantity", totalQuantityOnHand);
                    } else {
                        try {
                            targetInventoryItemList = ((GenericValue) ((Map<String, Object>) moveInfo).get("targetProductFacilityLocation")).getRelated("InventoryItem", null, null, false);
                        } catch (Exception e) {
                            Debug.logError(e, "Error getting related InventoryItem: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                        targetTotalAvailableToPromise = 0;
                        targetTotalQuantityOnHand = 0;
                        if (targetInventoryItemList != null) {
                            for (GenericValue inventoryItem_iter : targetInventoryItemList) {
                                inventoryItem = inventoryItem_iter;
                                targetTotalAvailableToPromise = new BigDecimal(inventoryItem.get("availableToPromiseTotal").toString());
                                targetTotalQuantityOnHand = new BigDecimal(inventoryItem.get("quantityOnHandTotal").toString());
                            }
                        }
                        moveInfo.put("availableToPromiseTotalTo", targetTotalAvailableToPromise);
                        moveInfo.put("quantityOnHandTotalTo", targetTotalQuantityOnHand);
                        if (targetTotalAvailableToPromise != null /* TODO: field compare operator less */) {
                            Map<String, Object> targetLocationSimpleMoveQuantity = new HashMap<String, Object>();
                            if (UtilValidate.isEmpty(((Map<String, Object>) targetLocationSimpleMoveQuantity).get(((Map<String, Object>) ((Map<String, Object>) moveInfo).get("targetProductFacilityLocation")).get("locationSeqId")))) {
                                ((Map<String, Object>) moveInfo).put("totalQuantity", new BigDecimal(((Map<String, Object>) ((Map<String, Object>) moveInfo).get("targetProductFacilityLocation")).get("moveQuantity").toString()));
                            } else {
                                ((Map<String, Object>) moveInfo).put("totalQuantity", new BigDecimal(((Map<String, Object>) targetLocationSimpleMoveQuantity).get(((Map<String, Object>) ((Map<String, Object>) moveInfo).get("targetProductFacilityLocation")).get("locationSeqId")).toString()));
                            }
                            if (totalQuantityOnHand != null /* TODO: field compare operator less */) {
                                ((Map<String, Object>) targetLocationSimpleMoveQuantity).put("moveInfo.targetProductFacilityLocation.locationSeqId", new BigDecimal(totalQuantityOnHand.toString()));
                                moveInfo.put("totalQuantity", totalQuantityOnHand);
                            }
                            stockMoveHandled.put((String) ((Map<String, Object>) ((Map<String, Object>) moveInfo).get("targetProductFacilityLocation")).get("locationSeqId"), "Y");
                        }
                    }
                    moveByOisgirInfoList.add(moveInfo);
                    moveInfo = new HashMap<String, Object>();
                }
            }
        }
        result.put("moveByOisgirInfoList", moveByOisgirInfoList);
        result.put("stockMoveHandled", stockMoveHandled);
        result.put("warningMessageList", warningMessageList);

        return result;
    }


    /**
     * Find all Stock Moves recommended to be done based on ProductFacilityLocation settings
     */
    public static Map<String, Object> findStockMovesRecommended(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue InventoryItemAndLocation = null;
        List<GenericValue> inventoryItemAndLocationList = null;
        Object totalQuantityOnHand = null;
        Object InventoryItemAndLocationByLocMap = null;
        GenericValue targetFacilityLocationSave = null;
        List<String> warningMessageList = null;
        Object fromLocationTotalAvailableToPromise = null;
        List<Object> InventoryItemAndLocationByLocMap_InventoryItemAndLocation_locationSeqId_ = null;
        Object fromLocationTotalAvailableToPromise_locationSeqId_ = null;
        Object targetLocationMoveQuantity = null;
        BigDecimal minimumStock = null;
        GenericValue productSave = null;
        List<Object> moveByPflInfoList = null;
        Object totalAvailableToPromise = null;
        Map<String, Object> moveInfo = null;
        Object stockMoveHandled = context.get("stockMoveHandled");
        productSave.put("productId", new HashMap<String, Object>());
        List<GenericValue> productFacilityLocationQuantityTestList = null;
        try {
            productFacilityLocationQuantityTestList = EntityQuery.use(delegator)
                    .from("ProductFacilityLocationQuantityTest")
                    .where(UtilMisc.toMap("locationTypeEnumId", "FLT_PICKLOC", "facilityId", context.get("facilityId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (productFacilityLocationQuantityTestList != null) {
            for (GenericValue productFacilityLocationQuantityTest : productFacilityLocationQuantityTestList) {
                minimumStock = (BigDecimal) productFacilityLocationQuantityTest.get("minimumStock");
                Object moveInfo_product = null;
                Object moveInfo_facilityLocationTo = null;
                Object moveInfo_availableToPromiseTotalTo = null;
                Object moveInfo_quantityOnHandTotalTo = null;
                Object moveInfo_availableToPromiseTotalFrom = null;
                Object moveInfo_quantityOnHandTotalFrom = null;
                Object moveInfo_totalQuantity = null;
                if ((!(UtilValidate.isEmpty(productFacilityLocationQuantityTest.get("moveQuantity"))) && ((Comparable) productFacilityLocationQuantityTest.get("moveQuantity")).compareTo(BigDecimal.ZERO) > 0 && ((UtilValidate.isEmpty(productFacilityLocationQuantityTest.get("availableToPromiseTotal")) && ((Comparable) minimumStock).compareTo(BigDecimal.ZERO) > 0) || (!(UtilValidate.isEmpty(productFacilityLocationQuantityTest.get("availableToPromiseTotal"))) && productFacilityLocationQuantityTest.get("availableToPromiseTotal") != null /* TODO: field compare operator less */)))) {
                    if ((UtilValidate.isEmpty(((Map<String, Object>) stockMoveHandled).get(productFacilityLocationQuantityTest.get("locationSeqId"))) || !"Y".equals(((Map<String, Object>) stockMoveHandled).get(productFacilityLocationQuantityTest.get("locationSeqId"))))) {
                        if (!java.util.Objects.equals(productFacilityLocationQuantityTest.get("productId"), productSave.get("productId"))) {
                            try {
                                productSave = productFacilityLocationQuantityTest.getRelatedOne("Product", false);
                            } catch (Exception e) {
                                Debug.logError(e, "Error getting related one Product: " + e.getMessage(), MODULE);
                                return ServiceUtil.returnError(e.getMessage());
                            }
                            fromLocationTotalAvailableToPromise = null;
                        }
                        try {
                            targetFacilityLocationSave = productFacilityLocationQuantityTest.getRelatedOne("FacilityLocation", false);
                        } catch (Exception e) {
                            Debug.logError(e, "Error getting related one FacilityLocation: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                        try {
                            inventoryItemAndLocationList = EntityQuery.use(delegator)
                                    .from("InventoryItemAndLocation")
                                    .where(UtilMisc.toMap("productId", productFacilityLocationQuantityTest.get("productId"), "facilityId", productFacilityLocationQuantityTest.get("facilityId"), "locationTypeEnumId", "FLT_BULK"))
                                    .queryList();
                        } catch (Exception e) {
                            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                        GenericValue inventoryItem = null;
                        Object perLocationInventoryItemAndLocList = null;
                        Object locationSeqId = null;
                        if (UtilValidate.isEmpty(inventoryItemAndLocationList)) {
                            warningMessageList.add("Error in stock move, could not find a bulk location for facility [" + productFacilityLocationQuantityTest.get("facilityId") + "] and product [" + productFacilityLocationQuantityTest.get("productId") + "]");
                        } else {
                            targetLocationMoveQuantity = productFacilityLocationQuantityTest.get("moveQuantity");
                            InventoryItemAndLocationByLocMap = null;
                            if (inventoryItemAndLocationList != null) {
                                for (GenericValue InventoryItemAndLocation_iter : inventoryItemAndLocationList) {
                                    InventoryItemAndLocation = InventoryItemAndLocation_iter;
                                    InventoryItemAndLocationByLocMap_InventoryItemAndLocation_locationSeqId_.add(InventoryItemAndLocation);
                                }
                            }
                            locationSeqId = null;
                            perLocationInventoryItemAndLocList = null;
                            for (Map.Entry<String, Object> entry : ((Map<String, Object>) InventoryItemAndLocationByLocMap).entrySet()) {
                                locationSeqId = entry.getKey();
                                perLocationInventoryItemAndLocList = entry.getValue();
                                if (UtilValidate.isEmpty(((Map<String, Object>) fromLocationTotalAvailableToPromise).get(locationSeqId))) {
                                    totalQuantityOnHand = 0;
                                    totalAvailableToPromise = 0;
                                    if (perLocationInventoryItemAndLocList != null) {
                                        for (Object inventoryItemEntry : (List<?>) perLocationInventoryItemAndLocList) {
                                            totalQuantityOnHand = new BigDecimal(((Map<String, Object>) inventoryItemEntry).get("quantityOnHandTotal").toString());
                                            totalAvailableToPromise = new BigDecimal(((Map<String, Object>) inventoryItemEntry).get("availableToPromiseTotal").toString());
                                        }
                                    }
                                } else {
                                    totalAvailableToPromise = ((Map<String, Object>) fromLocationTotalAvailableToPromise).get(locationSeqId);
                                }
                                if ((((Comparable) totalAvailableToPromise).compareTo(BigDecimal.ZERO) > 0 && ((Comparable) targetLocationMoveQuantity).compareTo(BigDecimal.ZERO) > 0)) {
                                    moveInfo.put("product", productSave);
                                    moveInfo.put("facilityLocationTo", targetFacilityLocationSave);
                                    InventoryItemAndLocation = EntityUtil.getFirst((List<GenericValue>) perLocationInventoryItemAndLocList);
                                    GenericValue moveInfo_facilityLocationFrom = null;
                                    try {
                                        moveInfo_facilityLocationFrom = InventoryItemAndLocation.getRelatedOne("FacilityLocation", false);
                                    } catch (Exception e) {
                                        Debug.logError(e, "Error getting related one FacilityLocation: " + e.getMessage(), MODULE);
                                        return ServiceUtil.returnError(e.getMessage());
                                    }
                                    GenericValue moveInfo_targetProductFacilityLocation = null;
                                    try {
                                        moveInfo_targetProductFacilityLocation = productFacilityLocationQuantityTest.getRelatedOne("ProductFacilityLocation", false);
                                    } catch (Exception e) {
                                        Debug.logError(e, "Error getting related one ProductFacilityLocation: " + e.getMessage(), MODULE);
                                        return ServiceUtil.returnError(e.getMessage());
                                    }
                                    moveInfo.put("availableToPromiseTotalTo", productFacilityLocationQuantityTest.get("availableToPromiseTotal"));
                                    moveInfo.put("quantityOnHandTotalTo", productFacilityLocationQuantityTest.get("quantityOnHandTotal"));
                                    moveInfo.put("availableToPromiseTotalFrom", totalAvailableToPromise);
                                    moveInfo.put("quantityOnHandTotalFrom", totalQuantityOnHand);
                                    if (totalAvailableToPromise != null /* TODO: field compare operator less */) {
                                        targetLocationMoveQuantity = new BigDecimal(totalAvailableToPromise.toString());
                                        moveInfo.put("totalQuantity", totalAvailableToPromise);
                                        ((Map<String, Object>) fromLocationTotalAvailableToPromise).put("locationSeqId", 0);
                                    } else {
                                        moveInfo.put("totalQuantity", targetLocationMoveQuantity);
                                        ((Map<String, Object>) fromLocationTotalAvailableToPromise).put("locationSeqId", new BigDecimal(targetLocationMoveQuantity.toString()));
                                        targetLocationMoveQuantity = 0;
                                    }
                                    moveByPflInfoList.add(moveInfo);
                                    moveInfo = new HashMap<String, Object>();
                                }
                            }
                        }
                    }
                }
            }
        }
        result.put("moveByPflInfoList", moveByPflInfoList);
        result.put("warningMessageList", warningMessageList);

        return result;
    }


    /**
     * Process a Physical Stock Move from one FacilityLocation to another, in the same Facility
     */
    public static Map<String, Object> processPhysicalStockMove(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Security security = dctx.getSecurity();
        List<String> error_list = new LinkedList<>();

        Object currentQuantityToMove = null;
        GenericValue targetInventoryItem = null;
        Map<String, Object> createInventoryItemMap = null;
        Object createNonOisgirTargetDetailMap = null;
        List<GenericValue> inventoryItemList = null;
        Object createNonOisgirDetailMap = null;
        List<String> warningMessageList = null;
        Object createOisgirDetailMap = null;
        Object createOisgirTargetDetailMap = null;
        GenericValue orderItemShipGrpInvResAndItemLocation = null;
        GenericValue inventoryItem = null;
        List<Object> oiirailByInvItemMap_orderItemShipGrpInvResAndItemLocation_inventoryItemId_ = null;
        BigDecimal quantityNotAvailableToMove = null;
        BigDecimal reservedQuantityLeftOver = null;
        Object quantityLeftToProcess = null;
        Object oiirailByInvItemMap = null;
        Object haveSetIiDetail = null;
        GenericValue orderItemShipGrpInvRes = null;
        GenericValue targetOrderItemShipGrpInvRes = null;
        List<GenericValue> orderItemShipGrpInvResAndItemLocationList = null;
        Object remainingQuantityOnHand = null;
        if (!security.hasEntityPermission("FACILITY", "_CREATE", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("ProductUiLabels", "ProductFacilityCreatePermissionError", locale));
        }
        if (!security.hasEntityPermission("FACILITY", "_UPDATE", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("ProductUiLabels", "ProductFacilityUpdatePermissionError", locale));
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        quantityLeftToProcess = context.get("quantityMoved");
        warningMessageList = (List<String>) context.get("warningMessageList");
        Object orderItemStatusId = "ITEM_APPROVED";
        context.put("createOisgirDetailMap", createOisgirDetailMap);
        context.put("createOisgirTargetDetailMap", createOisgirTargetDetailMap);
        context.put("orderItemShipGrpInvResAndItemLocation", orderItemShipGrpInvResAndItemLocation);
        context.put("inventoryItem", inventoryItem);
        context.put("quantityNotAvailableToMove", quantityNotAvailableToMove);
        context.put("reservedQuantityLeftOver", reservedQuantityLeftOver);
        context.put("quantityLeftToProcess", quantityLeftToProcess);
        context.put("orderItemStatusId", orderItemStatusId);
        context.put("oiirailByInvItemMap", oiirailByInvItemMap);
        context.put("targetInventoryItem", targetInventoryItem);
        context.put("currentQuantityToMove", currentQuantityToMove);
        context.put("haveSetIiDetail", haveSetIiDetail);
        context.put("orderItemShipGrpInvRes", orderItemShipGrpInvRes);
        context.put("targetOrderItemShipGrpInvRes", targetOrderItemShipGrpInvRes);
        context.put("orderItemShipGrpInvResAndItemLocationList", orderItemShipGrpInvResAndItemLocationList);
        context.put("remainingQuantityOnHand", remainingQuantityOnHand);
        processOisgirMoveByStatusInline(dctx, context);
        oiirailByInvItemMap_orderItemShipGrpInvResAndItemLocation_inventoryItemId_ = (List<Object>) context.get("oiirailByInvItemMap_orderItemShipGrpInvResAndItemLocation_inventoryItemId_");
        createInventoryItemMap = (Map<String, Object>) context.get("createInventoryItemMap");
        orderItemStatusId = "ITEM_CREATED";
        context.put("createOisgirDetailMap", createOisgirDetailMap);
        context.put("createOisgirTargetDetailMap", createOisgirTargetDetailMap);
        context.put("orderItemShipGrpInvResAndItemLocation", orderItemShipGrpInvResAndItemLocation);
        context.put("inventoryItem", inventoryItem);
        context.put("quantityNotAvailableToMove", quantityNotAvailableToMove);
        context.put("reservedQuantityLeftOver", reservedQuantityLeftOver);
        context.put("quantityLeftToProcess", quantityLeftToProcess);
        context.put("orderItemStatusId", orderItemStatusId);
        context.put("oiirailByInvItemMap", oiirailByInvItemMap);
        context.put("targetInventoryItem", targetInventoryItem);
        context.put("currentQuantityToMove", currentQuantityToMove);
        context.put("haveSetIiDetail", haveSetIiDetail);
        context.put("orderItemShipGrpInvRes", orderItemShipGrpInvRes);
        context.put("targetOrderItemShipGrpInvRes", targetOrderItemShipGrpInvRes);
        context.put("orderItemShipGrpInvResAndItemLocationList", orderItemShipGrpInvResAndItemLocationList);
        context.put("remainingQuantityOnHand", remainingQuantityOnHand);
        processOisgirMoveByStatusInline(dctx, context);
        oiirailByInvItemMap_orderItemShipGrpInvResAndItemLocation_inventoryItemId_ = (List<Object>) context.get("oiirailByInvItemMap_orderItemShipGrpInvResAndItemLocation_inventoryItemId_");
        createInventoryItemMap = (Map<String, Object>) context.get("createInventoryItemMap");
        if (((Comparable) quantityLeftToProcess).compareTo(BigDecimal.ZERO) > 0) {
            try {
                inventoryItemList = EntityQuery.use(delegator)
                        .from("InventoryItem")
                        .where(UtilMisc.toMap("productId", context.get("productId"), "facilityId", context.get("facilityId"), "locationSeqId", context.get("locationSeqId")))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (inventoryItemList != null) {
                for (GenericValue inventoryItem_iter : inventoryItemList) {
                    inventoryItem = inventoryItem_iter;
                    if (((Comparable) quantityLeftToProcess).compareTo(BigDecimal.ZERO) > 0) {
                        if (((Comparable) inventoryItem.get("availableToPromiseTotal")).compareTo(BigDecimal.ZERO) > 0) {
                            if (quantityLeftToProcess != null /* TODO: field compare operator greater */) {
                                currentQuantityToMove = inventoryItem.get("availableToPromiseTotal");
                            } else {
                                currentQuantityToMove = quantityLeftToProcess;
                            }
                            targetInventoryItem = delegator.makeValue("InventoryItem");
                            targetInventoryItem.put("locationSeqId", context.get("targetLocationSeqId"));
                            createNonOisgirTargetDetailMap = null;
                            // set-service-fields from "targetInventoryItem" to "createInventoryItemMap" for service "createInventoryItem"
                            createInventoryItemMap.putAll(UtilMisc.toMap(targetInventoryItem));
                            try {
                                Map<String, Object> serviceResult = dispatcher.runSync("createInventoryItem", createInventoryItemMap);
                                if (ServiceUtil.isError(serviceResult)) {
                                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                                }
                                ((Map<String, Object>) createNonOisgirTargetDetailMap).put("inventoryItemId", serviceResult.get("inventoryItemId"));
                            } catch (Exception e) {
                                Debug.logError(e, "Error calling createInventoryItem: " + e.getMessage(), MODULE);
                                return ServiceUtil.returnError(e.getMessage());
                            }
                            ((Map<String, Object>) createNonOisgirTargetDetailMap).put("availableToPromiseDiff", currentQuantityToMove);
                            ((Map<String, Object>) createNonOisgirTargetDetailMap).put("quantityOnHandDiff", currentQuantityToMove);
                            try {
                                Map<String, Object> serviceResult = dispatcher.runSync("createInventoryItemDetail", (Map<String, Object>) createNonOisgirTargetDetailMap);
                                if (ServiceUtil.isError(serviceResult)) {
                                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                                }
                            } catch (Exception e) {
                                Debug.logError(e, "Error calling createInventoryItemDetail: " + e.getMessage(), MODULE);
                                return ServiceUtil.returnError(e.getMessage());
                            }
                            createNonOisgirDetailMap = new HashMap<String, Object>();
                            ((Map<String, Object>) createNonOisgirDetailMap).put("inventoryItemId", inventoryItem.get("inventoryItemId"));
                            ((Map<String, Object>) createNonOisgirDetailMap).put("availableToPromiseDiff", new BigDecimal(currentQuantityToMove.toString()));
                            ((Map<String, Object>) createNonOisgirDetailMap).put("quantityOnHandDiff", new BigDecimal(currentQuantityToMove.toString()));
                            try {
                                Map<String, Object> serviceResult = dispatcher.runSync("createInventoryItemDetail", (Map<String, Object>) createNonOisgirDetailMap);
                                if (ServiceUtil.isError(serviceResult)) {
                                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                                }
                            } catch (Exception e) {
                                Debug.logError(e, "Error calling createInventoryItemDetail: " + e.getMessage(), MODULE);
                                return ServiceUtil.returnError(e.getMessage());
                            }
                            try {
                                targetInventoryItem.refresh();
                            } catch (Exception e) {
                                Debug.logError(e, "Error refreshing value: " + e.getMessage(), MODULE);
                                return ServiceUtil.returnError(e.getMessage());
                            }
                            Debug.logInfo("Just created new targetInventoryItem from non-OISGIR (ie location level based) [" + targetInventoryItem + "]", MODULE);
                        }
                    }
                }
            }
        }
        if (((Comparable) quantityLeftToProcess).compareTo(BigDecimal.ZERO) > 0) {
            warningMessageList.add("ERROR: Not enough available inventory found in location [" + context.get("locationSeqId") + "] in facility [" + context.get("facilityId") + "], did not reallocate " + quantityLeftToProcess + " of the " + context.get("quantityMoved") + " reported as physically moved.");
        }
        result.put("warningMessageList", warningMessageList);

        return result;
    }


    /**
     * Inline method to process OISGIR stock move for a specific OrderItem.statusId
     */
    public static Map<String, Object> processOisgirMoveByStatusInline(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue orderItemShipGrpInvResAndItemLocation = null;
        List<Object> oiirailByInvItemMap_orderItemShipGrpInvResAndItemLocation_inventoryItemId_ = null;
        Object createOisgirDetailMap = null;
        Object createOisgirTargetDetailMap = null;
        GenericValue inventoryItem = null;
        BigDecimal quantityNotAvailableToMove = null;
        BigDecimal reservedQuantityLeftOver = null;
        Object quantityLeftToProcess = null;
        GenericValue targetInventoryItem = null;
        Map<String, Object> createInventoryItemMap = null;
        Object currentQuantityToMove = null;
        Object haveSetIiDetail = null;
        GenericValue orderItemShipGrpInvRes = null;
        GenericValue targetOrderItemShipGrpInvRes = null;
        Object remainingQuantityOnHand = null;
        List<GenericValue> orderItemShipGrpInvResAndItemLocationList = null;
        try {
            orderItemShipGrpInvResAndItemLocationList = EntityQuery.use(delegator)
                    .from("OrderItemShipGrpInvResAndItemLocation")
                    .where(UtilMisc.toMap("productId", context.get("productId"), "facilityId", context.get("facilityId"), "locationSeqId", context.get("locationSeqId"), "orderItemStatusId", context.get("orderItemStatusId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Object oiirailByInvItemMap = null;
        Debug.logInfo("In processOisgirMoveByStatusInline orderItemShipGrpInvResAndItemLocationList=" + orderItemShipGrpInvResAndItemLocationList, MODULE);
        if (orderItemShipGrpInvResAndItemLocationList != null) {
            for (GenericValue orderItemShipGrpInvResAndItemLocation_iter : orderItemShipGrpInvResAndItemLocationList) {
                orderItemShipGrpInvResAndItemLocation = orderItemShipGrpInvResAndItemLocation_iter;
                oiirailByInvItemMap_orderItemShipGrpInvResAndItemLocation_inventoryItemId_.add(orderItemShipGrpInvResAndItemLocation);
            }
        }
        for (Map.Entry<String, Object> entry : ((Map<String, Object>) oiirailByInvItemMap).entrySet()) {
            String inventoryItemId = entry.getKey();
            orderItemShipGrpInvResAndItemLocationList = (List<GenericValue>) entry.getValue();
            try {
                inventoryItem = EntityQuery.use(delegator)
                        .from("InventoryItem")
                        .where(context)
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying InventoryItem: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if ("SERIALIZED_INV_ITEM".equals(inventoryItem.get("inventoryItemTypeId"))) {
                inventoryItem.put("locationSeqId", context.get("targetLocationSeqId"));
                try {
                    delegator.store(inventoryItem);
                } catch (Exception e) {
                    Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
            if ("NON_SERIAL_INV_ITEM".equals(inventoryItem.get("inventoryItemTypeId"))) {
                targetInventoryItem = delegator.makeValue("InventoryItem");
                targetInventoryItem.put("locationSeqId", context.get("targetLocationSeqId"));
                // set-service-fields from "targetInventoryItem" to "createInventoryItemMap" for service "createInventoryItem"
                createInventoryItemMap.putAll(UtilMisc.toMap(targetInventoryItem));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createInventoryItem", createInventoryItemMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                    targetInventoryItem.put("inventoryItemId", serviceResult.get("inventoryItemId"));
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createInventoryItem: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                try {
                    targetInventoryItem.refresh();
                } catch (Exception e) {
                    Debug.logError(e, "Error refreshing value: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                haveSetIiDetail = "N";
                remainingQuantityOnHand = inventoryItem.get("quantityOnHandTotal");
                if (orderItemShipGrpInvResAndItemLocationList != null) {
                    for (GenericValue orderItemShipGrpInvResAndItemLocation_iter : orderItemShipGrpInvResAndItemLocationList) {
                        orderItemShipGrpInvResAndItemLocation = orderItemShipGrpInvResAndItemLocation_iter;
                        try {
                            orderItemShipGrpInvRes = orderItemShipGrpInvResAndItemLocation.getRelatedOne("OrderItemShipGrpInvRes", false);
                        } catch (Exception e) {
                            Debug.logError(e, "Error getting related one OrderItemShipGrpInvRes: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                        reservedQuantityLeftOver = null;
                        currentQuantityToMove = null;
                        quantityNotAvailableToMove = null;
                        if (((Comparable) quantityLeftToProcess).compareTo(0D) > 0) {
                            if (quantityLeftToProcess != null /* TODO: field compare operator less */) {
                                currentQuantityToMove = quantityLeftToProcess;
                            } else {
                                currentQuantityToMove = orderItemShipGrpInvRes.get("quantity");
                            }
                            if (currentQuantityToMove != null /* TODO: field compare operator greater */) {
                                currentQuantityToMove = remainingQuantityOnHand;
                            }
                            if (UtilValidate.isEmpty(orderItemShipGrpInvRes.get("quantityNotAvailable"))) {
                                orderItemShipGrpInvRes.put("quantityNotAvailable", BigDecimal.ZERO);
                            }
                            reservedQuantityLeftOver = new BigDecimal(orderItemShipGrpInvRes.get("quantity").toString());
                            if ((!(UtilValidate.isEmpty(reservedQuantityLeftOver)) && ((Comparable) reservedQuantityLeftOver).compareTo(BigDecimal.ZERO) > 0)) {
                                quantityNotAvailableToMove = BigDecimal.ZERO;
                            } else {
                                quantityNotAvailableToMove = (BigDecimal) orderItemShipGrpInvRes.get("quantityNotAvailable");
                            }
                            targetOrderItemShipGrpInvRes = delegator.makeValue("OrderItemShipGrpInvRes");
                            targetOrderItemShipGrpInvRes.put("inventoryItemId", targetInventoryItem.get("inventoryItemId"));
                            createOisgirDetailMap = null;
                            createOisgirTargetDetailMap = null;
                            ((Map<String, Object>) createOisgirDetailMap).put("inventoryItemId", inventoryItem.get("inventoryItemId"));
                            ((Map<String, Object>) createOisgirTargetDetailMap).put("inventoryItemId", targetInventoryItem.get("inventoryItemId"));
                            ((Map<String, Object>) createOisgirDetailMap).put("quantityOnHandDiff", new BigDecimal(currentQuantityToMove.toString()));
                            ((Map<String, Object>) createOisgirTargetDetailMap).put("quantityOnHandDiff", new BigDecimal(currentQuantityToMove.toString()));
                            if (UtilValidate.isNotEmpty(orderItemShipGrpInvRes.get("quantityNotAvailable"))) {
                                if (((Comparable) orderItemShipGrpInvRes.get("quantityNotAvailable")).compareTo(BigDecimal.ZERO) > 0) {
                                    ((Map<String, Object>) createOisgirDetailMap).put("availableToPromiseDiff", new BigDecimal(quantityNotAvailableToMove.toString()));
                                    ((Map<String, Object>) createOisgirTargetDetailMap).put("availableToPromiseDiff", new BigDecimal(quantityNotAvailableToMove.toString()));
                                }
                            }
                            try {
                                Map<String, Object> serviceResult = dispatcher.runSync("createInventoryItemDetail", (Map<String, Object>) createOisgirDetailMap);
                                if (ServiceUtil.isError(serviceResult)) {
                                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                                }
                            } catch (Exception e) {
                                Debug.logError(e, "Error calling createInventoryItemDetail: " + e.getMessage(), MODULE);
                                return ServiceUtil.returnError(e.getMessage());
                            }
                            try {
                                Map<String, Object> serviceResult = dispatcher.runSync("createInventoryItemDetail", (Map<String, Object>) createOisgirTargetDetailMap);
                                if (ServiceUtil.isError(serviceResult)) {
                                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                                }
                            } catch (Exception e) {
                                Debug.logError(e, "Error calling createInventoryItemDetail: " + e.getMessage(), MODULE);
                                return ServiceUtil.returnError(e.getMessage());
                            }
                            targetOrderItemShipGrpInvRes.set("quantity", new BigDecimal(currentQuantityToMove.toString()));
                            targetOrderItemShipGrpInvRes.set("quantityNotAvailable", new BigDecimal(quantityNotAvailableToMove.toString()));
                            try {
                                delegator.create(targetOrderItemShipGrpInvRes);
                            } catch (Exception e) {
                                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                                return ServiceUtil.returnError(e.getMessage());
                            }
                            orderItemShipGrpInvRes.set("quantity", new BigDecimal(orderItemShipGrpInvRes.get("quantity").toString()));
                            orderItemShipGrpInvRes.set("quantityNotAvailable", new BigDecimal(orderItemShipGrpInvRes.get("quantityNotAvailable").toString()));
                            try {
                                delegator.store(orderItemShipGrpInvRes);
                            } catch (Exception e) {
                                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                                return ServiceUtil.returnError(e.getMessage());
                            }
                            if ((java.util.Objects.equals(orderItemShipGrpInvRes.get("quantity"), BigDecimal.ZERO) && java.util.Objects.equals(orderItemShipGrpInvRes.get("quantityNotAvailable"), BigDecimal.ZERO))) {
                                try {
                                    delegator.removeValue(orderItemShipGrpInvRes);
                                } catch (Exception e) {
                                    Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                                    return ServiceUtil.returnError(e.getMessage());
                                }
                            } else {
                                try {
                                    delegator.store(orderItemShipGrpInvRes);
                                } catch (Exception e) {
                                    Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                                    return ServiceUtil.returnError(e.getMessage());
                                }
                            }
                            haveSetIiDetail = "Y";
                            quantityLeftToProcess = new BigDecimal(quantityLeftToProcess.toString());
                            remainingQuantityOnHand = new BigDecimal(remainingQuantityOnHand.toString());
                        }
                    }
                }
                if ("N".equals(haveSetIiDetail)) {
                    Debug.logInfo("Did not end up finding an OISGIR that we could move inventory with, so removing targetInventoryItem: " + targetInventoryItem, MODULE);
                    try {
                        delegator.removeValue(targetInventoryItem);
                    } catch (Exception e) {
                        Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            }
        }

        return result;
    }


    /**
     * Find Product's inventory locations from facility
     */
    public static Map<String, Object> findProductInventorylocations(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<GenericValue> LocationList = null;
        try {
            LocationList = EntityQuery.use(delegator)
                    .from("InventoryItemAndLocation")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying InventoryItemAndLocation: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        result.put("LocationList", LocationList);

        return result;
    }

}

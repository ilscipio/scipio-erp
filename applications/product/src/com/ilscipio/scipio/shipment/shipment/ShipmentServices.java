/*******************************************************************************
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *******************************************************************************/
package com.ilscipio.scipio.shipment.shipment;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilNumber;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.minilang.SimpleMethod;
import org.ofbiz.minilang.method.MethodContext;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceContext;
import org.ofbiz.service.ServiceUtil;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * SCIPIO custom ShipmentServices
 */
public class ShipmentServices {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static final String resource = "ProductUiLabels";
    public static final String resource_error = "OrderErrorUiLabels";
    public static final int decimals = UtilNumber.getBigDecimalScale("order.decimals");
    public static final RoundingMode rounding = UtilNumber.getRoundingMode("order.rounding");
    public static final BigDecimal ZERO = BigDecimal.ZERO.setScale(decimals, rounding);

    public static Map<String, Object> orderCompleteShip(ServiceContext context) {
        Map<String, Object> result = new HashMap<String, Object>();
        Delegator delegator = context.delegator();
        LocalDispatcher dispatcher = context.dispatcher();
        Locale locale = context.locale();
        List<GenericValue> storeAll = new LinkedList<GenericValue>();
        GenericValue userLogin = context.userLogin();

        String orderId = context.getString("orderId");
        String shipGroupSeqId = context.getString("shipGroupSeqId");

        try {
            GenericValue orderHeader = delegator.findOne("OrderHeader", UtilMisc.toMap("orderId", orderId), false);
            if (UtilValidate.isEmpty(orderHeader) || (UtilValidate.isNotEmpty(orderHeader) && UtilValidate.isEmpty(orderHeader.getString("productStoreId")))) {
                return ServiceUtil.returnError(UtilProperties.getMessage(resource, "FacilityShipmentMissingProductStore", locale));
            }

            List<GenericValue> primaryShipments = orderHeader.getRelated("PrimaryShipment", null, null, false);
            GenericValue productStore = orderHeader.getRelatedOne("ProductStore", true);

            List<GenericValue> orderItemShipGroupList = orderHeader.getRelated("OrderItemShipGroup");
            Map<String, GenericValue> orderItemListByShGrpMap = getOrderItemShipGroupLists(orderId, delegator, locale);

            if (UtilValidate.isEmpty(primaryShipments)) {
                if (!productStore.getBoolean("reserveInventory")) {
                    return ServiceUtil.returnError(UtilProperties.getMessage(resource, "FacilityShipmentNotCreatedForNotReserveInventory", locale));
                }
                if (productStore.getBoolean("explodeOrderItems")) {
                    return ServiceUtil.returnError(UtilProperties.getMessage(resource, "FacilityShipmentNotCreatedForExplodesOrderItems", locale));
                }

                // locate shipping facilities associated with order item rez's -->
                List<String> orderItemShipGrpInvResFacilityIds = UtilMisc.newList();
                List<GenericValue> orderItemAndShipGrpInvResAndItemList = delegator.findByAnd("OrderItemAndShipGrpInvResAndItem", UtilMisc.toMap("orderId", orderId, "statusId", "ITEM_APPROVED"), null, false);
                for (GenericValue orderItemAndShipGrpInvResAndItem : orderItemAndShipGrpInvResAndItemList) {
                    if (!orderItemShipGrpInvResFacilityIds.contains(orderItemAndShipGrpInvResAndItem.getString("facilityId"))) {
                        orderItemShipGrpInvResFacilityIds.add(orderItemAndShipGrpInvResAndItem.getString("facilityId"));
                    }
                }

                for (String orderItemShipGrpInvResFacilityId : orderItemShipGrpInvResFacilityIds) {
                    GenericValue facility = delegator.findOne("Facility", UtilMisc.toMap("facilityId", orderItemShipGrpInvResFacilityId), true);
                    if (UtilValidate.isNotEmpty(facility)) {
                        Map<String, Object> mcCtx = context.context();
                        mcCtx.putAll(UtilMisc.toMap("orderHeader", orderHeader, "productStore", productStore, "orderItemListByShGrpMap", orderItemListByShGrpMap, "orderItemShipGroupList", orderItemShipGroupList));
                        MethodContext mc = new MethodContext(dispatcher.getDispatchContext(), mcCtx, context.dctx().getClassLoader());
                        mc.putAllEnv(mcCtx);
                        String createShipmentForFacilityAndShipGroupResult = SimpleMethod.runSimpleMethod("component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml", "createShipmentForFacilityAndShipGroup", mc);
                        if (!createShipmentForFacilityAndShipGroupResult.equals("success")) {
                            result = ServiceUtil.returnError(UtilProperties.getMessage(resource, "FacilityShipmentNotCreated", locale));
                        }
                    }
                }
            } else {
                for (GenericValue shipment : primaryShipments) {
                    List<GenericValue> itemIssuances = delegator.findByAnd("ItemIssuance",
                            UtilMisc.toMap("orderId", orderId, "shipmentId", shipment.getString("shipmentId"), "shipGroupSeqId", shipGroupSeqId),
                            UtilMisc.toList("issuedDateTime DESC"), false);
                    if (UtilValidate.isEmpty(itemIssuances)) {

                    }

                    for (GenericValue itemIssuance : itemIssuances) {
                        Debug.log("ItemIssuance: " + itemIssuance);
                    }
                }

                Map<String, Object> changeOrderStatusCtx = UtilMisc.toMap("orderId", orderId, "statusId", "ORDER_COMPLETED", "setItemStatus", "Y", "userLogin", userLogin);
                Map<String, Object> changeOrderStatusResult = dispatcher.runSync("changeOrderStatus", changeOrderStatusCtx);
                Debug.log("changeOrderStatusResult: " + changeOrderStatusResult);
            }
        } catch (Exception e) {
            result = ServiceUtil.returnError(UtilProperties.getMessage(resource, "FacilityShipmentMissingProductStore", locale));
        }

        if (ServiceUtil.isSuccess(result)) {
            Debug.logInfo("Finished orderCompleteShip:\nshipmentShipGroupFacilityList=${shipmentShipGroupFacilityList}\nsuccessMessageList=${successMessageList}", module);
        }

        return result;
    }


    public static Map<String, GenericValue> getOrderItemShipGroupLists(String orderId, Delegator delegator, Locale locale) throws Exception {
        // short-description="Sub-method used by quickShip methods to get a list of OrderItemAndShipGroupAssoc and a Map of shipGroupId -> OrderItemAndShipGroupAssoc">
        List<GenericValue> orderItemAndShipGroupAssocList = delegator.findByAnd("OrderItemAndShipGroupAssoc",
                UtilMisc.toMap("orderId", orderId, "statusId", "ITEM_APPROVED"), null, false);

        // lookup all the approved items, doing by item because the item must be approved before shipping
        // make sure we have something to ship
        if (UtilValidate.isEmpty(orderItemAndShipGroupAssocList)) {
            throw new Exception(UtilProperties.getMessage(resource, "FacilityNoItemsAvailableToShip", locale));
        }

        // group orderItems (actually OrderItemAndShipGroupAssocs) by shipGroupSeqId in a Map with List values
        // This Map is actually used only for sales orders' shipments right now.
        Map<String, GenericValue> orderItemListByShGrpMap = UtilMisc.newMap();
        for (GenericValue orderItemAndShipGroupAssoc : orderItemAndShipGroupAssocList) {
            orderItemListByShGrpMap.put(orderItemAndShipGroupAssoc.getString("shipGroupSeqId"), orderItemAndShipGroupAssoc);
        }
        return orderItemListByShGrpMap;
    }


}

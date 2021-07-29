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
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.order.order.OrderReadHelper;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceContext;
import org.ofbiz.service.ServiceUtil;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.TimeZone;

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

    public static Map<String, Object> orderSendShip(ServiceContext context) {
        Map<String, Object> result = new HashMap<String, Object>();
        Delegator delegator = context.delegator();
        LocalDispatcher dispatcher = context.dispatcher();
        Locale locale = context.locale();
        TimeZone timeZone = context.timeZone();
        GenericValue userLogin = context.userLogin();

        String orderId = context.getString("orderId");

        try {
            GenericValue orderHeader = delegator.findOne("OrderHeader", UtilMisc.toMap("orderId", orderId), false);
            if (UtilValidate.isEmpty(orderHeader) || (UtilValidate.isNotEmpty(orderHeader) && UtilValidate.isEmpty(orderHeader.getString("productStoreId")))) {
                return ServiceUtil.returnError(UtilProperties.getMessage(resource, "FacilityShipmentMissingProductStore", locale));
            }

            OrderReadHelper orh = new OrderReadHelper(orderHeader);
            List<EntityCondition> shipmentConditions = UtilMisc.toList(
                    EntityCondition.makeCondition("statusId", EntityOperator.IN,
                            UtilMisc.toList("SHIPMENT_INPUT", "SHIPMENT_SCHEDULED", "SHIPMENT_PICKED", "SHIPMENT_PACKED")),
                    EntityCondition.makeCondition("primaryOrderId", EntityOperator.EQUALS, orderId)
            );

            List<GenericValue> shipments = EntityQuery.use(delegator).from("Shipment").where(shipmentConditions).cache(false).queryList();
            if (UtilValidate.isNotEmpty(shipments)) {
                Map<String, List<GenericValue>> orderItemsNotIssuedPerShipment = findOrderItemsNotIssuedPerShipment(shipments, orh);
                if (UtilValidate.isNotEmpty(orderItemsNotIssuedPerShipment)) {
                    // TODO: Force issue items? Review
                    orderHeader.set("needsInventoryIssuance", "Y");
                    orderHeader.store();
                }
                Map<String, Object> changeOrderStatusCtx = UtilMisc.toMap("orderId", orderId, "statusId", "ORDER_SENT", "setItemStatus", "Y", "userLogin", userLogin);
                Map<String, Object> changeOrderStatusResult = dispatcher.runSync("changeOrderStatus", changeOrderStatusCtx);
                Debug.log("changeOrderStatusResult: " + changeOrderStatusResult);

                if (ServiceUtil.isSuccess(changeOrderStatusCtx)) {
                    for (GenericValue shipment : shipments) {
                        Map<String, Object> updateShipmentCtx = UtilMisc.toMap("shipmentId", shipment.getString("shipmentId"),
                                "primaryOrderId", orderId, "statusId", "SHIPMENT_SHIPPED", "userLogin", userLogin, "timeZone", timeZone);
                        Map<String, Object> updateShipmentResponse = dispatcher.runSync("updateShipment", updateShipmentCtx);
                        if (!ServiceUtil.isSuccess(updateShipmentResponse)) {
                            // TODO: Handle this situation
                        }
                    }
                }
            }

        } catch (Exception e) {
            result = ServiceUtil.returnError(UtilProperties.getMessage(resource, "FacilityShipmentMissingProductStore", locale));
        }

        if (ServiceUtil.isSuccess(result)) {
            Debug.logInfo("Finished orderSendShip:\nshipmentShipGroupFacilityList=${shipmentShipGroupFacilityList}\nsuccessMessageList=${successMessageList}", module);
        }

        return result;
    }

    public static Map<String, Object> orderCompleteShip(ServiceContext context) {
        Map<String, Object> result = new HashMap<String, Object>();
        Delegator delegator = context.delegator();
        LocalDispatcher dispatcher = context.dispatcher();
        Locale locale = context.locale();
        GenericValue userLogin = context.userLogin();

        String orderId = context.getString("orderId");

        try {
            GenericValue orderHeader = delegator.findOne("OrderHeader", UtilMisc.toMap("orderId", orderId), false);
            if (UtilValidate.isEmpty(orderHeader) || (UtilValidate.isNotEmpty(orderHeader) && UtilValidate.isEmpty(orderHeader.getString("productStoreId")))) {
                return ServiceUtil.returnError(UtilProperties.getMessage(resource, "FacilityShipmentMissingProductStore", locale));
            }
            OrderReadHelper orh = new OrderReadHelper(orderHeader);

            List<EntityCondition> shipmentConditions = UtilMisc.toList(
                    EntityCondition.makeCondition("statusId", EntityOperator.IN,
                            UtilMisc.toList("SHIPMENT_INPUT", "SHIPMENT_SCHEDULED", "SHIPMENT_PICKED", "SHIPMENT_PACKED", "SHIPMENT_SHIPPED")),
                    EntityCondition.makeCondition("primaryOrderId", EntityOperator.EQUALS, orderId)
            );

            List<GenericValue> shipments = EntityQuery.use(delegator).from("Shipment").where(shipmentConditions).cache(false).queryList();
            if (UtilValidate.isNotEmpty(shipments)) {
                Map<String, List<GenericValue>> orderItemsNotIssuedPerShipment = findOrderItemsNotIssuedPerShipment(shipments, orh);
                if (UtilValidate.isNotEmpty(orderItemsNotIssuedPerShipment)) {
                    // TODO: Force issue items?
                    orderHeader.set("needsInventoryIssuance", "Y");
                    orderHeader.store();
                }

                Map<String, Object> changeOrderStatusCtx = UtilMisc.toMap("orderId", orderId, "statusId", "ORDER_COMPLETED", "setItemStatus", "Y", "userLogin", userLogin);
                Map<String, Object> changeOrderStatusResult = dispatcher.runSync("changeOrderStatus", changeOrderStatusCtx);
                Debug.log("changeOrderStatusResult: " + changeOrderStatusResult);
                if (ServiceUtil.isSuccess(changeOrderStatusCtx)) {
                    for (GenericValue shipment : shipments) {
                        Map<String, Object> updateShipmentCtx = UtilMisc.toMap("shipmentId", shipment.getString("shipmentId"), "statusId", "SHIPMENT_DELIVERED", "userLogin", userLogin);
                        Map<String, Object> updateShipmentResponse = dispatcher.runSync("updateShipment", updateShipmentCtx);
                        if (!ServiceUtil.isSuccess(updateShipmentResponse)) {
                            // TODO: Handle this situation
                        }
                    }
                }
            } else {
                // TODO: Throw error?
            }
        } catch (Exception e) {
            result = ServiceUtil.returnError(UtilProperties.getMessage(resource, "FacilityShipmentMissingProductStore", locale));
        }

        if (ServiceUtil.isSuccess(result)) {
            Debug.logInfo("Finished orderCompleteShip:\nshipmentShipGroupFacilityList=${shipmentShipGroupFacilityList}\nsuccessMessageList=${successMessageList}", module);
        }

        return result;
    }

    private static Map<String, List<GenericValue>> findOrderItemsNotIssuedPerShipment(List<GenericValue> shipments, OrderReadHelper orh) {
        Map<String, List<GenericValue>> orderItemsNotIssuedPerShipment = UtilMisc.newMap();
        for (GenericValue shipment : shipments) {
            List<GenericValue> orderItemsNotIssued = UtilMisc.newList();
            for (GenericValue orderItem : orh.getOrderItems()) {
                List<GenericValue> itemIssuances = orh.getOrderItemIssuances(orderItem, shipment.getString("shipmentId"));

                BigDecimal totalIssuedQuantity = BigDecimal.ZERO;
                for (GenericValue itemIssuance : itemIssuances) {
                    totalIssuedQuantity = totalIssuedQuantity.add(itemIssuance.getBigDecimal("quantity"));
                    BigDecimal cancelledQuantity = itemIssuance.getBigDecimal("cancelQuantity");
                    if (UtilValidate.isNotEmpty(cancelledQuantity)) {
                        totalIssuedQuantity = totalIssuedQuantity.subtract(cancelledQuantity);
                    }
                }
                if (orderItem.getBigDecimal("quantity").compareTo(totalIssuedQuantity) != 0) {
                    orderItemsNotIssued.add(orderItem);
                }
            }
            if (UtilValidate.isNotEmpty(orderItemsNotIssued)) {
                orderItemsNotIssuedPerShipment.put(shipment.getString("shipmentId"), orderItemsNotIssued);
            }
        }
        return orderItemsNotIssuedPerShipment;
    }
}

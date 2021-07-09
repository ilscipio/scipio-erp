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
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.minilang.SimpleMethod;
import org.ofbiz.minilang.method.MethodContext;
import org.ofbiz.order.order.OrderReadHelper;
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

    public static Map<String, Object> orderSendShip(ServiceContext context) {
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

            EntityCondition shipmentCondition = EntityCondition.makeCondition("statusId", EntityOperator.IN,
                    UtilMisc.toList("SHIPMENT_INPUT", "SHIPMENT_SCHEDULED", "SHIPMENT_PICKED", "SHIPMENT_PACKED"));
            List<GenericValue> primaryShipments = EntityQuery.use(delegator).from("PrimaryShipment").where(shipmentCondition).cache(false).queryList();
            if (UtilValidate.isNotEmpty(primaryShipments)) {
                Map<String, List<GenericValue>> orderItemsNotIssuedPerShipment = UtilMisc.newMap();
                for (GenericValue shipment : primaryShipments) {
                    List<GenericValue> orderItemsNotIssued = UtilMisc.newList();
                    for (GenericValue orderItem : orh.getOrderItems()) {
                        List<GenericValue> itemIssuances = orh.getOrderItemIssuances(orderItem, shipment.getString("shipmentId"));

                        BigDecimal totalIssuedQuantity = BigDecimal.ZERO;
                        for (GenericValue itemIssuance : itemIssuances) {
                            totalIssuedQuantity.add(itemIssuance.getBigDecimal("quantity").subtract(itemIssuance.getBigDecimal("cancelQuantity")));
                        }
                        if (orderItem.getBigDecimal("quantity").compareTo(totalIssuedQuantity) != 0) {
                            orderItemsNotIssued.add(orderItem);
                        }
                    }
                    orderItemsNotIssuedPerShipment.put(shipment.getString("shipmentId"), orderItemsNotIssued);
                }

                if (UtilValidate.isNotEmpty(orderItemsNotIssuedPerShipment)) {
                    // TODO: Force issue items?
                }


                Map<String, Object> changeOrderStatusCtx = UtilMisc.toMap("orderId", orderId, "statusId", "ORDER_SENT", "setItemStatus", "Y", "userLogin", userLogin);
                Map<String, Object> changeOrderStatusResult = dispatcher.runSync("changeOrderStatus", changeOrderStatusCtx);
                Debug.log("changeOrderStatusResult: " + changeOrderStatusResult);
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

            EntityCondition shipmentCondition = EntityCondition.makeCondition("statusId", EntityOperator.IN,
                    UtilMisc.toList("SHIPMENT_INPUT", "SHIPMENT_SCHEDULED", "SHIPMENT_PICKED", "SHIPMENT_PACKED", "SHIPMENT_SHIPPED"));
            List<GenericValue> primaryShipments = EntityQuery.use(delegator).from("PrimaryShipment").where(shipmentCondition).cache(false).queryList();
            if (UtilValidate.isNotEmpty(primaryShipments)) {
                Map<String, List<GenericValue>> orderItemsNotIssuedPerShipment = UtilMisc.newMap();
                for (GenericValue shipment : primaryShipments) {
                    List<GenericValue> orderItemsNotIssued = UtilMisc.newList();
                    for (GenericValue orderItem : orh.getOrderItems()) {
                        List<GenericValue> itemIssuances = orh.getOrderItemIssuances(orderItem, shipment.getString("shipmentId"));

                        BigDecimal totalIssuedQuantity = BigDecimal.ZERO;
                        for (GenericValue itemIssuance : itemIssuances) {
                            totalIssuedQuantity.add(itemIssuance.getBigDecimal("quantity").subtract(itemIssuance.getBigDecimal("cancelQuantity")));
                        }
                        if (orderItem.getBigDecimal("quantity").compareTo(totalIssuedQuantity) != 0) {
                            orderItemsNotIssued.add(orderItem);
                        }
                    }
                    orderItemsNotIssuedPerShipment.put(shipment.getString("shipmentId"), orderItemsNotIssued);
                }

                if (UtilValidate.isNotEmpty(orderItemsNotIssuedPerShipment)) {
                    // TODO: Force issue items?
                }

                Map<String, Object> changeOrderStatusCtx = UtilMisc.toMap("orderId", orderId, "statusId", "ORDER_COMPLETED", "setItemStatus", "Y", "userLogin", userLogin);
                result = dispatcher.runSync("changeOrderStatus", changeOrderStatusCtx);
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


}

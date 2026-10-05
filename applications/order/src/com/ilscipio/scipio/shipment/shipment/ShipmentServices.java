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

                if (ServiceUtil.isSuccess(changeOrderStatusResult)) {
                    for (GenericValue shipment : shipments) {
                        Map<String, Object> updateShipmentResponse = ServiceUtil.returnSuccess();
                        // We need to change to SHIPMENT_PACKED first so we can change to SHIPMENT_SHIPPED eventually (there might be a more appropriate approach to this though)
                        if (!shipment.getString("statusId").equals("SHIPMENT_PACKED")) {
                            Map<String, Object> updateShipmentCtx = UtilMisc.toMap("shipmentId", shipment.getString("shipmentId"),
                                    "primaryOrderId", orderId, "statusId", "SHIPMENT_PACKED", "userLogin", userLogin, "timeZone", timeZone);
                            updateShipmentResponse = dispatcher.runSync("updateShipment", updateShipmentCtx);
                        }
                        if (ServiceUtil.isSuccess(updateShipmentResponse)) {
                            Map<String, Object> updateShipmentCtx = UtilMisc.toMap("shipmentId", shipment.getString("shipmentId"),
                                    "primaryOrderId", orderId, "statusId", "SHIPMENT_SHIPPED", "userLogin", userLogin, "timeZone", timeZone);
                            updateShipmentResponse = dispatcher.runSync("updateShipment", updateShipmentCtx);
                        }
                        if (!ServiceUtil.isSuccess(updateShipmentResponse)) {
                            Debug.logError("Unable to send order: " + ServiceUtil.getErrorMessage(updateShipmentResponse), module);
                            result = ServiceUtil.returnError(ServiceUtil.getErrorMessage(updateShipmentResponse));
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

            for (GenericValue orderItemShipGroup : orh.getOrderItemShipGroups()) {
                Debug.log("orderItemShipGroup: " + orderItemShipGroup);

                List<GenericValue> shipments = null;
                boolean isNoShipping = orderItemShipGroup.getString("shipmentMethodTypeId").equals("NO_SHIPPING");
                if (!isNoShipping) {
                    List<EntityCondition> shipmentConditions = UtilMisc.toList(
                            EntityCondition.makeCondition("statusId", EntityOperator.IN,
                                    UtilMisc.toList("SHIPMENT_INPUT", "SHIPMENT_SCHEDULED", "SHIPMENT_PICKED", "SHIPMENT_PACKED", "SHIPMENT_SHIPPED")),
                            EntityCondition.makeCondition("primaryOrderId", EntityOperator.EQUALS, orderId)
                    );
                    shipments = EntityQuery.use(delegator).from("Shipment").where(shipmentConditions).cache(false).queryList();
                }

                if (UtilValidate.isNotEmpty(shipments) || isNoShipping) {
                    Map<String, List<GenericValue>> orderItemsNotIssuedPerShipment = findOrderItemsNotIssuedPerShipment(shipments, orh);
                    if (UtilValidate.isNotEmpty(orderItemsNotIssuedPerShipment)) {
                        // TODO: Force issue items?
                        orderHeader.set("needsInventoryIssuance", "Y");
                        orderHeader.store();
                    }

                    Map<String, Object> changeOrderStatusCtx = UtilMisc.toMap("orderId", orderId, "statusId", "ORDER_COMPLETED", "setItemStatus", "Y", "userLogin", userLogin);
                    Map<String, Object> changeOrderStatusResult = dispatcher.runSync("changeOrderStatus", changeOrderStatusCtx);
                    if (ServiceUtil.isSuccess(changeOrderStatusResult)) {
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

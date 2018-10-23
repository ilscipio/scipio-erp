/*
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
 */

import org.ofbiz.base.util.Debug
import org.ofbiz.entity.condition.*

shipmentId = parameters.shipmentId;
shipment = from("Shipment").where("shipmentId", shipmentId).queryOne();

// orderHeader is needed here to determine type of order and hence types of shipment status
if (!shipment) {
    primaryOrderId = request.getParameter("primaryOrderId");
} else {
    primaryOrderId = shipment.primaryOrderId;
}
orderHeader = from("OrderHeader").where("orderId" : primaryOrderId).queryOne();

facilityList = [];
if (orderHeader) {    
    if (orderHeader.productStoreId) {
        productStoreFacilityList = delegator.findByAnd("ProductStoreFacilityByOrder", ["orderId" : orderHeader.orderId, "productStoreId" : orderHeader.productStoreId], null, false);
        context.productStoreFacilityList = productStoreFacilityList;
        context.productStoreId = orderHeader.productStoreId;
    }
} 
facilityList = delegator.findByAnd("Facility", null, null, false);
context.facilityList = facilityList;

// the kind of StatusItem to use is based on the type of order
statusItemTypeId = "SHIPMENT_STATUS";
if (orderHeader && "PURCHASE_ORDER".equals(orderHeader.orderTypeId)) {
    statusItemTypeId = "PURCH_SHIP_STATUS";
}
context.statusItemTypeId = statusItemTypeId;

context.shipmentId = shipmentId;
context.shipment = shipment;

shipmentTypeList = from("ShipmentType").queryList();
context.shipmentTypeList = shipmentTypeList;

uomList = from("Uom").where(["uomTypeId" : "CURRENCY_MEASURE"]).orderBy("description").queryList();
context.uomList = uomList;

if (shipment) {
    currentStatus = shipment.getRelatedOne("StatusItem", false);
    originPostalAddress = shipment.getRelatedOne("OriginPostalAddress", false);
    destinationPostalAddress = shipment.getRelatedOne("DestinationPostalAddress", false);
    originTelecomNumber = shipment.getRelatedOne("OriginTelecomNumber", false);
    destinationTelecomNumber = shipment.getRelatedOne("DestinationTelecomNumber", false);
    
    

    context.currentStatus = currentStatus;
    context.originPostalAddress = originPostalAddress;
    context.destinationPostalAddress = destinationPostalAddress;
    context.originTelecomNumber = originTelecomNumber;
    context.destinationTelecomNumber = destinationTelecomNumber;

}
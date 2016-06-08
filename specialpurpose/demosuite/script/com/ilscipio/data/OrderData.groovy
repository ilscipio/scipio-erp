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

import java.sql.Timestamp

import org.ofbiz.base.util.Debug
import org.ofbiz.base.util.UtilMisc
import org.ofbiz.base.util.UtilProperties
import org.ofbiz.base.util.UtilRandom
import org.ofbiz.entity.*
import org.ofbiz.entity.util.*
import org.ofbiz.service.ServiceUtil

public Map createDemoOrder() {
    List<String> orderTypes = [
        "PURCHASE_ORDER",
        "SALES_ORDER"
    ]
    List<String> orderStatusTypes = [
        "ORDER_CREATED",
        "ORDER_COMPLETED"
    ];
    
    List<String> products = [
            ["productId":"CAM-2644","itemDescription":"Nikon Analog Camera","unitPrice":38.4,"unitListPrice":48.0],
            ["productId":"CDR-1111","itemDescription":"CD-R Writable Discs","unitPrice":59.99,"unitListPrice":60.0]
        ];
    
    
    totalProductCount = from("Product").queryCount();
    totalProductCountInt = (totalProductCount < Integer.MAX_VALUE) ? (int) totalProductCount : Integer.MAX_VALUE - 1;
    Debug.log("totalProductCount ====> " + totalProductCountInt);
    
    Debug.logInfo("-=-=-=- DEMO DATA CREATION SERVICE - ORDER DATA-=-=-=-", "");
    Map result = ServiceUtil.returnSuccess();
    
    List<GenericValue> toBeStored = new ArrayList<GenericValue>();
    List<GenericValue> orderItems = new ArrayList<GenericValue>();
    int num = context.num;
    
    for(int i = 0; i <num; i++){
        // Create OrderHeader
        String orderId = "GEN_"+delegator.getNextSeqId("demo-orderheader");
        // Create OrderItem (between 1 and 3)
        int orderItemCount = UtilRandom.getRandomInt(1,3);
        BigDecimal remainingSubTotal = new BigDecimal(0.00);
        BigDecimal grandTotal = new BigDecimal(0.00);
        
        for (int orderItemSeqId = 1; orderItemSeqId <= orderItemCount; orderItemSeqId++) {
//            Map product = products[UtilRandom.getRandomInt(0,products.size()-1)];
            EntityFindOptions efo = new EntityFindOptions();
            efo.setMaxRows(1);
            efo.setOffset(UtilRandom.getRandomInt(0, totalProductCountInt));
            products = from("Product").query(efo);
            if (products) {
                product = products.get(0);
                Debug.log("Product ============> " + product);
                productPrices = product.getRelated("ProductPrice", null, null, false);
                defaultPrice = 0;
                listPrice = 0;
                for (productPrice in productPrices) {
                    if (productPrice.productPriceTypeId.equals("DEFAULT_PRICE"))
                        defaultPrice = productPrice.price;
                    else (productPrice.productPriceTypeId.equals("LIST_PRICE")) 
                        listPrice = productPrice.price;
                }
                
                String productId = product.productId;
                String prodCatalogId= "DemoCatalog";
                BigDecimal quantity = new BigDecimal(UtilRandom.getRandomInt(0,10));
                BigDecimal unitPrice= new BigDecimal(defaultPrice);
                BigDecimal unitListPrice= new BigDecimal(listPrice);
                BigDecimal selectedAmount = new BigDecimal(0.0);
                BigDecimal itemCost = BigDecimal.ZERO;
                itemCost = unitPrice.multiply(new BigDecimal(quantity));
                remainingSubTotal =  remainingSubTotal.add(itemCost);
                grandTotal = grandTotal.add(itemCost);
                
                fields = UtilMisc.toMap("orderId", orderId,"orderItemSeqId","0000"+orderItemSeqId,"orderItemTypeId","PRODUCT_ORDER_ITEM","productId",
                                        productId,"prodCatalogId",prodCatalogId,"isPromo","N","quantity",quantity,"selectedAmount",selectedAmount,
                                        "unitPrice",unitPrice,"unitListPrice",unitListPrice,"isModifiedPrice","N","itemDescription","Round Gizmo",
                                        "correspondingPoId","","statusId","ITEM_APPROVED");
            
                GenericValue orderItem = delegator.makeValue("OrderItem", fields);
                orderItems.add(orderItem);
            }
        }
        
        String orderTypeId = orderTypes.get(UtilRandom.random(orderTypes));
        String orderName="Demo Order";
        String salesChannelEnumId = "UNKNWN_SALES_CHANNEL";
        Timestamp orderDate = Timestamp.valueOf(UtilRandom.generateRandomDate(context));
        String statusId = orderStatusTypes.get(UtilRandom.random(orderStatusTypes));
        Map fields = UtilMisc.toMap("orderId", orderId,"orderTypeId",orderTypeId,"orderName",orderName,"salesChannelEnumId",
                                    salesChannelEnumId,"orderDate",orderDate,"priority","2","entryDate",orderDate,"statusId",statusId,
                                    "currencyUom","USD","webSiteId","OrderEntry","remainingSubTotal",remainingSubTotal,"grandTotal",grandTotal);
    
        GenericValue orderHeader = delegator.makeValue("OrderHeader", fields);
        toBeStored.add(orderHeader);
        toBeStored.addAll(orderItems);
        
        // Create orderRole
        fields = UtilMisc.toMap("orderId", orderId,"partyId","Company","roleTypeId","BILL_FROM_VENDOR");
        GenericValue orderRole1 = delegator.makeValue("OrderRole", fields);
        toBeStored.add(orderRole1);
        
        fields = UtilMisc.toMap("orderId", orderId,"partyId","DemoCustomer","roleTypeId","BILL_TO_CUSTOMER");    
        GenericValue orderRole2 = delegator.makeValue("OrderRole", fields);
        toBeStored.add(orderRole2);
    
        fields = UtilMisc.toMap("orderId", orderId,"partyId","DemoCustomer","roleTypeId","END_USER_CUSTOMER");
        GenericValue orderRole3 = delegator.makeValue("OrderRole", fields);
        toBeStored.add(orderRole3);
    
        fields = UtilMisc.toMap("orderId", orderId,"partyId","DemoCustomer","roleTypeId","PLACING_CUSTOMER");
        GenericValue orderRole4 = delegator.makeValue("OrderRole", fields);
        toBeStored.add(orderRole4);
    
        fields = UtilMisc.toMap("orderId", orderId,"partyId","DemoCustomer","roleTypeId","SHIP_TO_CUSTOMER");
        GenericValue orderRole5 = delegator.makeValue("OrderRole", fields);
        toBeStored.add(orderRole5);
        
        // Create OrderStatus    
        String orderStatusId = "GEN_"+delegator.getNextSeqId("demo-orderstatusid");
        fields = UtilMisc.toMap("orderId", orderId,"orderStatusId",orderStatusId,"statusId","ORDER_CREATED",
                                "statusDatetime",orderDate,"statusUserLogin","admin");
        GenericValue orderStatus = delegator.makeValue("OrderStatus", fields);
        toBeStored.add(orderStatus);
        
        if(UtilRandom.getRandomBoolean()==true){
        orderStatusId = "GEN_"+delegator.getNextSeqId("demo-orderstatusid");
        fields = UtilMisc.toMap("orderId", orderId,"orderStatusId",orderStatusId,"statusId","ORDER_COMPLETED",
                                "statusDatetime",orderDate,"statusUserLogin","admin");
        GenericValue orderStatus2 = delegator.makeValue("OrderStatus", fields);
        toBeStored.add(orderStatus2);
        }
    }
    
    // store the changes
    if (toBeStored.size() > 0) {
        try {
            delegator.storeAll(toBeStored);
            result.put("generatedData", toBeStored);
        } catch (GenericEntityException e) {
            return ServiceUtil.returnError(UtilProperties.getMessage(resource_error,
            "OrderErrorCannotStoreStatusChanges", locale) + e.getMessage());
        }
    }
    
   
    
    return result;
}
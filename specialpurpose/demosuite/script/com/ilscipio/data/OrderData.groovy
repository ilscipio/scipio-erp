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

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.ofbiz.service.ServiceUtil;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.entity.*;
import org.ofbiz.entity.util.*;
import java.text.SimpleDateFormat;
import java.sql.Timestamp;

public Map createDemoOrder() {
	List<String> orderTypes = [
		"PURCHASE_ORDER",
		"SALES_ORDER"
	]
	List<String> orderStatusTypes = [
		"ORDER_CREATED",
		"ORDER_COMPLETED"
	]
	
	Debug.logInfo("-=-=-=- DEMO DATA CREATION SERVICE - ORDER DATA-=-=-=-", "");
	Map result = ServiceUtil.returnSuccess();
	
	
	List<GenericValue> toBeStored = new ArrayList<GenericValue>();
	int num = context.num;
	
	for(int i = 0; i <num; i++){
		// Create OrderHeader
		String orderId = "GEN_"+delegator.getNextSeqId("demo-orderheader");
		String orderTypeId = orderTypes.get(random(orderTypes));
		String orderName="Demo Order";
		String salesChannelEnumId = "UNKNWN_SALES_CHANNEL";
		Timestamp orderDate = Timestamp.valueOf(generateRandomDate());
		String statusId = orderStatusTypes.get(random(orderStatusTypes));
		BigDecimal remainingSubTotal = new BigDecimal(48.00);
		BigDecimal grandTotal = new BigDecimal(48.00);
	
		Map fields = UtilMisc.toMap("orderId", orderId,"orderTypeId",orderTypeId,"orderName",orderName,"salesChannelEnumId",
									salesChannelEnumId,"orderDate",orderDate,"priority","2","entryDate",orderDate,"statusId",statusId,
									"currencyUom","USD","webSiteId","OrderEntry","remainingSubTotal",remainingSubTotal,"grandTotal",grandTotal);
	
		GenericValue orderHeader = delegator.makeValue("OrderHeader", fields);
		toBeStored.add(orderHeader);
	
		// Create OrderItem (only 1 for now)
		String productId = "GZ-2644";
		String prodCatalogId="DemoCatalog";
		//float quantity = 2.0;
		BigDecimal unitPrice= new BigDecimal(38.4);
		BigDecimal unitListPrice= new BigDecimal(48.0);
		BigDecimal selectedAmount = new BigDecimal(0.0);
		
		fields = UtilMisc.toMap("orderId", orderId,"orderItemSeqId","00001","orderItemTypeId","PRODUCT_ORDER_ITEM","productId",
								productId,"prodCatalogId",prodCatalogId,"isPromo","N","quantity",2.0,"selectedAmount",selectedAmount,
								"unitPrice",unitPrice,"unitListPrice",unitListPrice,"isModifiedPrice","N","itemDescription","Round Gizmo",
								"correspondingPoId","","statusId","ITEM_APPROVED");
	
		GenericValue orderItem = delegator.makeValue("OrderItem", fields);
		toBeStored.add(orderItem);
		
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
		
		if(getRandomBoolean()==true){
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
		} catch (GenericEntityException e) {
			return ServiceUtil.returnError(UtilProperties.getMessage(resource_error,
			"OrderErrorCannotStoreStatusChanges", locale) + e.getMessage());
		}
	}
	
	return result;
}

/**
 * Method should generate random number that represents
 * a time between two dates.
 *
 * @return
 */
private long getRandomTimeBetweenTwoDates () {
	Calendar cal = Calendar.getInstance();
	long endTime = cal.getTimeInMillis();
	cal.add(Calendar.DATE, -180);
	long beginTime = cal.getTimeInMillis();
	long diff = endTime - beginTime + 1;
	return beginTime + (long) (Math.random() * diff);
}



public String generateRandomDate() {
	SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss");
	Date randomDate = new Date(getRandomTimeBetweenTwoDates());
	return dateFormat.format(randomDate);
}

public int random(List myList){
	int size = myList.size();
	int index = new Random().nextInt(size);
	return index;
}

public static boolean getRandomBoolean() {
	return Math.random() < 0.5;
}
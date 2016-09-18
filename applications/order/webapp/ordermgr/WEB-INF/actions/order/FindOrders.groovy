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

import java.util.*;
import java.sql.Timestamp;
import org.ofbiz.entity.*;
import org.ofbiz.base.util.*;
import org.ofbiz.entity.util.*;

module = "FindOrders.groovy";

// get the order types
orderTypes = from("OrderType").orderBy("description").queryList();
context.orderTypes = orderTypes;

// get the role types
roleTypes = from("RoleType").orderBy("description").queryList();
context.roleTypes = roleTypes;

// get the order statuses
orderStatuses = from("StatusItem").where("statusTypeId", "ORDER_STATUS").orderBy("sequenceId", "description").queryList();
context.orderStatuses = orderStatuses;

// get websites
websites = from("WebSite").orderBy("siteName").queryList();
context.webSites = websites;

// get the stores
stores = from("ProductStore").orderBy("storeName").queryList();
context.productStores = stores;

// get the channels
channels = from("Enumeration").where("enumTypeId", "ORDER_SALES_CHANNEL").orderBy("sequenceId").queryList();
context.salesChannels = channels;

// get the Shipping Methods
carrierShipmentMethods = from("CarrierShipmentMethod").queryList();
context.carrierShipmentMethods = carrierShipmentMethods;

// get the Payment Status
paymentStatusList = from("StatusItem").where("statusTypeId", "PAYMENT_PREF_STATUS").orderBy("description").queryList();
context.paymentStatusList = paymentStatusList;

// get the good identification types
goodIdentificationTypes = from("GoodIdentificationType").orderBy("goodIdentificationTypeId", "description").queryList();
context.goodIdentificationTypes = goodIdentificationTypes;

// current role type
currentRoleTypeId = parameters.roleTypeId;
if (currentRoleTypeId) {
    // SCIPIO: roleTypeId supports multiple, so get only the first, and have a second list of IDs
    // Leave the existing fields as they were for compatibility.
    if (currentRoleTypeId instanceof List) {
        context.currentRoleTypeIdList = currentRoleTypeId;
        currentRoleTypeId = currentRoleTypeId[0];
    } else {
        context.currentRoleTypeIdList = [currentRoleTypeId];
    }
    currentRole = from("RoleType").where("roleTypeId", currentRoleTypeId).cache(true).queryOne();
    context.currentRole = currentRole;
}

// current selected type
currentTypeId = parameters.orderTypeId;
if (currentTypeId) {
    currentType = from("OrderType").where("orderTypeId", currentTypeId).cache(true).queryOne();
    context.currentType = currentType;
}
// current selected status
currentStatusId = parameters.orderStatusId;
if (currentStatusId) {
    currentStatus = from("StatusItem").where("statusId", currentStatusId).cache(true).queryOne();
    context.currentStatus = currentStatus;
}

// current website
currentWebSiteId = parameters.orderWebSiteId;
if (currentWebSiteId) {
    currentWebSite = from("WebSite").where("webSiteId", currentWebSiteId).cache(true).queryOne();
    context.currentWebSite = currentWebSite;
}

// current store
currentProductStoreId = parameters.productStoreId;
if (currentProductStoreId) {
    currentProductStore = from("ProductStore").where("productStoreId", currentProductStoreId).cache(true).queryOne();
    context.currentProductStore = currentProductStore;
}

// current Shipping Method
shipmentMethod = parameters.shipmentMethod;
if (shipmentMethod) {
    carrierPartyId = shipmentMethod.substring(0, shipmentMethod.indexOf("@"));
    shipmentMethodTypeId = shipmentMethod.substring(shipmentMethod.indexOf("@")+1);
    if (carrierPartyId && shipmentMethodTypeId) {
        currentCarrierShipmentMethod = from("CarrierShipmentMethod").where("partyId", carrierPartyId, "shipmentMethodTypeId", shipmentMethodTypeId).queryFirst();
        context.currentCarrierShipmentMethod = currentCarrierShipmentMethod;
    }
}

// current channel
currentSalesChannelId = parameters.salesChannelEnumId;
if (currentSalesChannelId) {
    currentSalesChannel = from("Enumeration").where("enumId", currentSalesChannelId).queryOne();
    context.currentSalesChannel = currentSalesChannel;
}

// current good identification type
currentGoodIdentificationTypeId = parameters.goodIdentificationTypeId;
if (currentGoodIdentificationTypeId) {
    currentGoodIdentificationType = from("GoodIdentificationType").where("goodIdentificationTypeId", currentGoodIdentificationTypeId).queryOne();
    context.currentGoodIdentificationType = currentGoodIdentificationType;
}

// create the fromDate for calendar
fromCal = Calendar.getInstance();
fromCal.setTime(new java.util.Date());
fromCal.set(Calendar.DAY_OF_WEEK, fromCal.getActualMinimum(Calendar.DAY_OF_WEEK));
fromCal.set(Calendar.HOUR_OF_DAY, fromCal.getActualMinimum(Calendar.HOUR_OF_DAY));
fromCal.set(Calendar.MINUTE, fromCal.getActualMinimum(Calendar.MINUTE));
fromCal.set(Calendar.SECOND, fromCal.getActualMinimum(Calendar.SECOND));
fromCal.set(Calendar.MILLISECOND, fromCal.getActualMinimum(Calendar.MILLISECOND));
fromTs = new Timestamp(fromCal.getTimeInMillis());
fromStr = fromTs.toString();
fromStr = fromStr.substring(0, fromStr.indexOf('.'));
context.fromDateStr = fromStr;

// create the thruDate for calendar
toCal = Calendar.getInstance();
toCal.setTime(new java.util.Date());
toCal.set(Calendar.DAY_OF_WEEK, toCal.getActualMaximum(Calendar.DAY_OF_WEEK));
toCal.set(Calendar.HOUR_OF_DAY, toCal.getActualMaximum(Calendar.HOUR_OF_DAY));
toCal.set(Calendar.MINUTE, toCal.getActualMaximum(Calendar.MINUTE));
toCal.set(Calendar.SECOND, toCal.getActualMaximum(Calendar.SECOND));
toCal.set(Calendar.MILLISECOND, toCal.getActualMaximum(Calendar.MILLISECOND));
toTs = new Timestamp(toCal.getTimeInMillis());
toStr = toTs.toString();
context.thruDateStr = toStr;

// set the page parameters
if(parameters.viewIndex){
    viewIndex = Integer.valueOf(parameters.viewIndex);
}else if(parameters.VIEW_INDEX){
    viewIndex = Integer.valueOf(parameters.VIEW_INDEX);
}else{
    viewIndex = 1;
}
context.viewIndex = viewIndex;

if(parameters.viewSize){
    viewSize = Integer.valueOf(parameters.viewSize);
}else if(parameters.VIEW_SIZE){
    viewSize = Integer.valueOf(parameters.VIEW_SIZE);
}else{
    viewSize = UtilProperties.getPropertyValue("widget", "widget.form.defaultViewSize", "20");
}
context.viewSize = viewSize;

// get the lookup flag
lookupFlag = parameters.lookupFlag;

// fields from the service call
paramList = request.getAttribute("paramList") ?: "";
context.paramList = paramList;

if (paramList) {
    // SCIPIO: FIXME: The paramlist should not be escaped this early; it should be escaped by Freemarker
    paramIds = paramList.split("&amp;");
    context.paramIdList = Arrays.asList(paramIds);
}

orderList = request.getAttribute("orderList");
context.orderList = orderList;
context.searchPerformed = (orderList != null); // can't do this from template

orderListSize = request.getAttribute("orderListSize");
context.orderListSize = orderListSize;

context.filterInventoryProblems = request.getAttribute("filterInventoryProblemsList");
context.filterPOsWithRejectedItems = request.getAttribute("filterPOsWithRejectedItemsList");
context.filterPOsOpenPastTheirETA = request.getAttribute("filterPOsOpenPastTheirETAList");
context.filterPartiallyReceivedPOs = request.getAttribute("filterPartiallyReceivedPOsList");

lowIndex = request.getAttribute("lowIndex");
context.lowIndex = lowIndex;

highIndex = request.getAttribute("highIndex");
context.highIndex = highIndex;

showAll = request.getAttribute("showAll");
context.showAll = showAll;

// SCIPIO: if mass change was submitted and there was no error but no success message,
// add a default success message.
if ("Y" == parameters.massOrderChangeSubmitted && !Boolean.TRUE.equals(context.isError)) {
    if (!context.eventMessageList) {
        context.eventMessageList = [UtilProperties.getMessage("CommonUiLabels", "CommonServiceSuccessMessage", locale)];
    }
}

// SCIPIO: if there was a find query error or any kind, never hide fields
if (context.isFindQueryError == Boolean.TRUE) {
    parameters.hideFields = "N";
}



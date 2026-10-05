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
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://product/script/org/ofbiz/product/inventory/InventoryIssueServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class InventoryIssueServices {

    private static final String MODULE = InventoryIssueServices.class.getName();


    /**
     * Issues the Inventory for an Order that was Immediately Fulfilled
     */
    public static Map<String, Object> issueImmediatelyFulfilledOrder(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Long iiCount = null;
        List<GenericValue> orderItemList = null;
        Map<String, Object> callSvcMap = null;
        GenericValue orderHeader = null;
        try {
            orderHeader = EntityQuery.use(delegator)
                    .from("OrderHeader")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderHeader: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue productStore = null;
        try {
            productStore = EntityQuery.use(delegator)
                    .from("ProductStore")
                    .where(UtilMisc.toMap("productStoreId", orderHeader.get("productStoreId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ProductStore: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(orderHeader)) {
            if ("Y".equals(orderHeader.get("needsInventoryIssuance"))) {
                try {
                    orderItemList = orderHeader.getRelated("OrderItem", null, null, false);
                } catch (Exception e) {
                    Debug.logError(e, "Error getting related OrderItem: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                iiCount = null;
                try {
                    iiCount = EntityQuery.use(delegator)
                            .from("InventoryItem")
                            .where("facilityId", orderHeader.get("originFacilityId"))
                            .queryCount();
                } catch (Exception e) {
                    Debug.logError(e, "Error counting InventoryItem: " + e.getMessage(), MODULE);
                }
                GenericValue orderItem = null;
                if (((Comparable) iiCount).compareTo(BigDecimal.ZERO) > 0) {
                    if (orderItemList != null) {
                        for (GenericValue orderItemEntry : orderItemList) {
                            if (UtilValidate.isNotEmpty(orderItemEntry.get("productId"))) {
                                callSvcMap = new HashMap<String, Object>();
                                // set-service-fields from "orderItem" to "callSvcMap" for service "issueImmediatelyFulfilledOrderItem"
                                callSvcMap.putAll(UtilMisc.toMap(orderItemEntry));
                                callSvcMap.put("orderHeader", orderHeader);
                                callSvcMap.put("orderItem", orderItemEntry);
                                callSvcMap.put("productStore", productStore);
                                try {
                                    Map<String, Object> serviceResult = dispatcher.runSync("issueImmediatelyFulfilledOrderItem", callSvcMap);
                                    if (ServiceUtil.isError(serviceResult)) {
                                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                                    }
                                } catch (Exception e) {
                                    Debug.logError(e, "Error calling issueImmediatelyFulfilledOrderItem: " + e.getMessage(), MODULE);
                                    return ServiceUtil.returnError(e.getMessage());
                                }
                            }
                        }
                    }
                    orderHeader.put("needsInventoryIssuance", "N");
                    try {
                        delegator.store(orderHeader);
                    } catch (Exception e) {
                        Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    Debug.logInfo("Issued inventory for orderId " + orderHeader.get("orderId") + ".", MODULE);
                } else {
                    Debug.logInfo("Not issuing inventory for orderId " + orderHeader.get("orderId") + ", no inventory information available.", MODULE);
                }
            }
        }

        return result;
    }


    /**
     * Issues the Inventory for an Order Item that was Immediately Fulfilled
     */
    public static Map<String, Object> issueImmediatelyFulfilledOrderItem(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue orderItem = null;
        List<Object> orderByList = null;
        Object createInvItemOutMap = null;
        Map<String, Object> lookupFieldMap = null;
        Map<String, Object> issuanceCreateMap = null;
        List<GenericValue> inventoryItemList = null;
        GenericValue orderHeader = null;
        Map<String, Object> inlineResult = null;
        Object itemIssuanceId = null;
        Object orderByString = null;
        Timestamp nowTimestamp = null;
        GenericValue productStore = null;
        Map<String, Object> createDetailMap = null;
        Object createInvItemInMap = null;
        Map<String, Object> inventoryItem = null;
        Map<String, Object> inventoryItemMap = null;
        Object lastNonSerInventoryItem = null;
        if (UtilValidate.isEmpty(context.get("orderItem"))) {
            try {
                orderItem = EntityQuery.use(delegator)
                        .from("OrderItem")
                        .where(context)
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying OrderItem: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        } else {
            orderItem = (GenericValue) context.get("orderItem");
        }
        if (UtilValidate.isNotEmpty(orderItem.get("productId"))) {
            nowTimestamp = new Timestamp(System.currentTimeMillis());
            if (UtilValidate.isEmpty(context.get("orderHeader"))) {
                try {
                    orderHeader = EntityQuery.use(delegator)
                            .from("OrderHeader")
                            .where(context)
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying OrderHeader: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            } else {
                orderHeader = (GenericValue) context.get("orderHeader");
            }
            if (UtilValidate.isEmpty(context.get("productStore"))) {
                try {
                    productStore = EntityQuery.use(delegator)
                            .from("ProductStore")
                            .where(UtilMisc.toMap("productStoreId", orderHeader.get("productStoreId")))
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying ProductStore: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            } else {
                productStore = (GenericValue) context.get("productStore");
            }
            if ("INVRO_FIFO_EXP".equals(productStore.get("reserveOrderEnumId"))) {
                orderByString = "+expireDate";
            } else {
                if ("INVRO_LIFO_EXP".equals(productStore.get("reserveOrderEnumId"))) {
                    orderByString = "-expireDate";
                } else {
                    if ("INVRO_LIFO_REC".equals(productStore.get("reserveOrderEnumId"))) {
                        orderByString = "-datetimeReceived";
                    } else {
                        orderByString = "+datetimeReceived";
                    }
                }
            }
            orderByList.add(orderByString);
            lookupFieldMap.put("productId", orderItem.get("productId"));
            lookupFieldMap.put("facilityId", orderHeader.get("originFacilityId"));
            try {
                inventoryItemList = EntityQuery.use(delegator)
                        .from("InventoryItem")
                        .where(lookupFieldMap)
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying InventoryItem: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            context.put("quantityNotIssued", orderItem.get("quantity"));
            if (inventoryItemList != null) {
                for (GenericValue inventoryItem_iter : inventoryItemList) {
                    inventoryItem = (Map<String, Object>) inventoryItem_iter;
                    context.put("inventoryItem", inventoryItem);
                    context.put("itemIssuanceId", itemIssuanceId);
                    context.put("issuanceCreateMap", issuanceCreateMap);
                    context.put("createDetailMap", createDetailMap);
                    context.put("lastNonSerInventoryItem", lastNonSerInventoryItem);
                    issueImmediateForInventoryItemInline(dctx, context);
                    inventoryItemMap = (Map<String, Object>) context.get("inventoryItemMap");
                    context = (Map<String, Object>) context.get("context");
                }
            }
            if (!java.util.Objects.equals(context.get("quantityNotIssued"), BigDecimal.ZERO)) {
                if (UtilValidate.isNotEmpty(lastNonSerInventoryItem)) {
                    issuanceCreateMap.put("orderId", context.get("orderId"));
                    issuanceCreateMap.put("orderItemSeqId", context.get("orderItemSeqId"));
                    issuanceCreateMap.put("inventoryItemId", ((Map<String, Object>) lastNonSerInventoryItem).get("inventoryItemId"));
                    issuanceCreateMap.put("quantity", context.get("quantityNotIssued"));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("createItemIssuance", issuanceCreateMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                        }
                        itemIssuanceId = serviceResult.get("itemIssuanceId");
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling createItemIssuance: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    createDetailMap.put("inventoryItemId", ((Map<String, Object>) lastNonSerInventoryItem).get("inventoryItemId"));
                    createDetailMap.put("orderId", context.get("orderId"));
                    createDetailMap.put("orderItemSeqId", context.get("orderItemSeqId"));
                    createDetailMap.put("itemIssuanceId", itemIssuanceId);
                    ((Map<String, Object>) createDetailMap).put("availableToPromiseDiff", new BigDecimal(context.get("quantityNotIssued").toString()));
                    ((Map<String, Object>) createDetailMap).put("quantityOnHandDiff", new BigDecimal(context.get("quantityNotIssued").toString()));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("createInventoryItemDetail", createDetailMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling createInventoryItemDetail: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    createDetailMap = new HashMap<String, Object>();
                    itemIssuanceId = null;
                } else {
                    createInvItemInMap = null;
                    createInvItemOutMap = null;
                    ((Map<String, Object>) createInvItemInMap).put("productId", orderItem.get("productId"));
                    ((Map<String, Object>) createInvItemInMap).put("facilityId", orderHeader.get("originFacilityId"));
                    ((Map<String, Object>) createInvItemInMap).put("inventoryItemTypeId", "NON_SERIAL_INV_ITEM");
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("createInventoryItem", (Map<String, Object>) createInvItemInMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                        }
                        ((Map<String, Object>) createInvItemOutMap).put("inventoryItemId", serviceResult.get("inventoryItemId"));
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling createInventoryItem: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    issuanceCreateMap.put("orderId", context.get("orderId"));
                    issuanceCreateMap.put("orderItemSeqId", context.get("orderItemSeqId"));
                    issuanceCreateMap.put("inventoryItemId", ((Map<String, Object>) createInvItemOutMap).get("inventoryItemId"));
                    issuanceCreateMap.put("quantity", context.get("quantityNotIssued"));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("createItemIssuance", issuanceCreateMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                        }
                        itemIssuanceId = serviceResult.get("itemIssuanceId");
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling createItemIssuance: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    createDetailMap.put("inventoryItemId", ((Map<String, Object>) createInvItemOutMap).get("inventoryItemId"));
                    createDetailMap.put("orderId", context.get("orderId"));
                    createDetailMap.put("orderItemSeqId", context.get("orderItemSeqId"));
                    createDetailMap.put("itemIssuanceId", itemIssuanceId);
                    ((Map<String, Object>) createDetailMap).put("availableToPromiseDiff", new BigDecimal(context.get("quantityNotIssued").toString()));
                    ((Map<String, Object>) createDetailMap).put("quantityOnHandDiff", new BigDecimal(context.get("quantityNotIssued").toString()));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("createInventoryItemDetail", createDetailMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling createInventoryItemDetail: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    createDetailMap = new HashMap<String, Object>();
                    itemIssuanceId = null;
                }
                ((Map<String, Object>) context).put("quantityNotIssued", 0);
            }
        }

        return result;
    }


    /**
     * Does a issuance for one InventoryItem, meant to be called in-line
     */
    public static Map<String, Object> issueImmediateForInventoryItemInline(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> inventoryItem = null;
        Map<String, Object> inventoryItemMap = null;
        Object itemIssuanceId = null;
        Map<String, Object> issuanceCreateMap = null;
        Map<String, Object> createDetailMap = null;
        Object lastNonSerInventoryItem = null;
        if (((Comparable) context.get("quantityNotIssued")).compareTo(BigDecimal.ZERO) > 0) {
            if ("SERIALIZED_INV_ITEM".equals(((Map<String, Object>) inventoryItem).get("inventoryItemTypeId"))) {
                if ("INV_AVAILABLE".equals(((Map<String, Object>) inventoryItem).get("statusId"))) {
                    inventoryItem.put("statusId", "INV_DELIVERED");
                    // set-service-fields from "inventoryItem" to "inventoryItemMap" for service "updateInventoryItem"
                    inventoryItemMap.putAll(UtilMisc.toMap(inventoryItem));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("updateInventoryItem", inventoryItemMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling updateInventoryItem: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    issuanceCreateMap.put("orderId", context.get("orderId"));
                    issuanceCreateMap.put("orderItemSeqId", context.get("orderItemSeqId"));
                    issuanceCreateMap.put("inventoryItemId", ((Map<String, Object>) inventoryItem).get("inventoryItemId"));
                    ((Map<String, Object>) issuanceCreateMap).put("quantity", 1);
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("createItemIssuance", issuanceCreateMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling createItemIssuance: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    issuanceCreateMap = new HashMap<String, Object>();
                    ((Map<String, Object>) context).put("quantityNotIssued", new BigDecimal(context.get("quantityNotIssued").toString()));
                }
            }
            if ("NON_SERIAL_INV_ITEM".equals(((Map<String, Object>) inventoryItem).get("inventoryItemTypeId"))) {
                Object parameters_deductAmount = null;
                Object issuanceCreateMap_orderId = null;
                Object issuanceCreateMap_orderItemSeqId = null;
                Object issuanceCreateMap_inventoryItemId = null;
                Object issuanceCreateMap_quantity = null;
                Object createDetailMap_inventoryItemId = null;
                Object createDetailMap_orderId = null;
                Object createDetailMap_orderItemSeqId = null;
                Object createDetailMap_itemIssuanceId = null;
                if (((UtilValidate.isEmpty(((Map<String, Object>) inventoryItem).get("statusId")) || "INV_AVAILABLE".equals(((Map<String, Object>) inventoryItem).get("statusId"))) && !(UtilValidate.isEmpty(((Map<String, Object>) inventoryItem).get("availableToPromiseTotal"))) && ((Comparable) ((Map<String, Object>) inventoryItem).get("availableToPromiseTotal")).compareTo(BigDecimal.ZERO) > 0)) {
                    if (context.get("quantityNotIssued") != null /* TODO: field compare operator greater */) {
                        context.put("deductAmount", ((Map<String, Object>) inventoryItem).get("availableToPromiseTotal"));
                    } else {
                        context.put("deductAmount", context.get("quantityNotIssued"));
                    }
                    issuanceCreateMap.put("orderId", context.get("orderId"));
                    issuanceCreateMap.put("orderItemSeqId", context.get("orderItemSeqId"));
                    issuanceCreateMap.put("inventoryItemId", ((Map<String, Object>) inventoryItem).get("inventoryItemId"));
                    issuanceCreateMap.put("quantity", context.get("deductAmount"));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("createItemIssuance", issuanceCreateMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                        }
                        itemIssuanceId = serviceResult.get("itemIssuanceId");
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling createItemIssuance: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    createDetailMap.put("inventoryItemId", ((Map<String, Object>) inventoryItem).get("inventoryItemId"));
                    createDetailMap.put("orderId", context.get("orderId"));
                    createDetailMap.put("orderItemSeqId", context.get("orderItemSeqId"));
                    createDetailMap.put("itemIssuanceId", itemIssuanceId);
                    ((Map<String, Object>) createDetailMap).put("availableToPromiseDiff", new BigDecimal(context.get("deductAmount").toString()));
                    ((Map<String, Object>) createDetailMap).put("quantityOnHandDiff", new BigDecimal(context.get("deductAmount").toString()));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("createInventoryItemDetail", createDetailMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling createInventoryItemDetail: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    createDetailMap = new HashMap<String, Object>();
                    ((Map<String, Object>) context).put("quantityNotIssued", new BigDecimal(context.get("deductAmount").toString()));
                    issuanceCreateMap = new HashMap<String, Object>();
                    itemIssuanceId = null;
                    lastNonSerInventoryItem = inventoryItem;
                }
            }
        }

        return result;
    }

}

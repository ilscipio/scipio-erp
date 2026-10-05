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
package com.ilscipio.scipio.order.event;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.LocalDispatcher;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://order/script/org/ofbiz/order/UpgradeServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class UpgradeServices {

    private static final String MODULE = UpgradeServices.class.getName();


    /**
     * Migrate data from OldOrderItemAssociation to OrderItemAssoc
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String migrateOrderItemAssociation(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue orderItemAssoc = null;
        List<GenericValue> oldOrderItemAssociations = null;
        try {
            oldOrderItemAssociations = EntityQuery.use(delegator)
                    .from("OldOrderItemAssociation")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OldOrderItemAssociation: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (oldOrderItemAssociations != null) {
            for (GenericValue oldOrderItemAssociation : oldOrderItemAssociations) {
                orderItemAssoc = delegator.makeValue("OrderItemAssoc");
                orderItemAssoc.put("orderId", ((Map<String, Object>) oldOrderItemAssociation).get("salesOrderId"));
                orderItemAssoc.put("orderItemSeqId", ((Map<String, Object>) oldOrderItemAssociation).get("soItemSeqId"));
                orderItemAssoc.put("toOrderId", ((Map<String, Object>) oldOrderItemAssociation).get("purchaseOrderId"));
                orderItemAssoc.put("toOrderItemSeqId", ((Map<String, Object>) oldOrderItemAssociation).get("poItemSeqId"));
                orderItemAssoc.put("shipGroupSeqId", "_NA_");
                orderItemAssoc.put("toShipGroupSeqId", "_NA_");
                orderItemAssoc.put("orderItemAssocTypeId", "PURCHASE_ORDER");
                try {
                    delegator.create(orderItemAssoc);
                } catch (Exception e) {
                    Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }

        return "success";
    }


    /**
     * Migrate data from OldCustRequestRole to CustRequestParty
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String migrateCustRequestRole(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue custRequestParty = null;
        List<GenericValue> oldCustRequestRoles = null;
        try {
            oldCustRequestRoles = EntityQuery.use(delegator)
                    .from("OldCustRequestRole")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OldCustRequestRole: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Timestamp fromDate = new Timestamp(System.currentTimeMillis());
        if (oldCustRequestRoles != null) {
            for (GenericValue oldCustRequestRole : oldCustRequestRoles) {
                custRequestParty = delegator.makeValue("CustRequestParty");
                custRequestParty.put("custRequestId", ((Map<String, Object>) oldCustRequestRole).get("custRequestId"));
                custRequestParty.put("partyId", ((Map<String, Object>) oldCustRequestRole).get("partyId"));
                custRequestParty.put("roleTypeId", ((Map<String, Object>) oldCustRequestRole).get("roleTypeId"));
                custRequestParty.put("fromDate", fromDate);
                try {
                    delegator.create(custRequestParty);
                } catch (Exception e) {
                    Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }

        return "success";
    }


    /**
     * Migrate data from ItemIssuances to OrderShipments when the records are not used to record item issuances but only an order to shipment association.
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String migrateOrderShipment(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue orderShipment = null;
        List<GenericValue> itemIssuanceRoles = null;
        List<GenericValue> inventoryItemDetails = null;
        List<GenericValue> itemIssuances = null;
        try {
            itemIssuances = EntityQuery.use(delegator)
                    .from("ItemIssuance")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ItemIssuance: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (itemIssuances != null) {
            for (GenericValue itemIssuance : itemIssuances) {
                try {
                    orderShipment = EntityQuery.use(delegator)
                            .from("OrderShipment")
                            .where(UtilMisc.toMap("orderId", ((Map<String, Object>) itemIssuance).get("orderId"), "orderItemSeqId", ((Map<String, Object>) itemIssuance).get("orderItemSeqId"), "shipGroupSeqId", ((Map<String, Object>) itemIssuance).get("shipGroupSeqId"), "shipmentId", ((Map<String, Object>) itemIssuance).get("shipmentId"), "shipmentItemSeqId", ((Map<String, Object>) itemIssuance).get("shipmentItemSeqId")))
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying OrderShipment: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (UtilValidate.isEmpty(orderShipment)) {
                    orderShipment = delegator.makeValue("OrderShipment");
                    orderShipment.put("orderId", ((Map<String, Object>) itemIssuance).get("orderId"));
                    orderShipment.put("orderItemSeqId", ((Map<String, Object>) itemIssuance).get("orderItemSeqId"));
                    orderShipment.put("shipGroupSeqId", ((Map<String, Object>) itemIssuance).get("shipGroupSeqId"));
                    orderShipment.put("shipmentId", ((Map<String, Object>) itemIssuance).get("shipmentId"));
                    orderShipment.put("shipmentItemSeqId", ((Map<String, Object>) itemIssuance).get("shipmentItemSeqId"));
                    orderShipment.put("quantity", new BigDecimal("0.0"));
                    try {
                        delegator.create(orderShipment);
                    } catch (Exception e) {
                        Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
                orderShipment.set("quantity", (new BigDecimal(((Map<String, Object>) orderShipment).get("quantity").toString())).add(new BigDecimal(((Map<String, Object>) itemIssuance).get("quantity").toString())));
                try {
                    delegator.store(orderShipment);
                } catch (Exception e) {
                    Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                try {
                    itemIssuanceRoles = itemIssuance.getRelated("ItemIssuanceRole", null, null, false);
                } catch (Exception e) {
                    Debug.logError(e, "Error getting related ItemIssuanceRole: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (itemIssuanceRoles != null) {
                    for (GenericValue itemIssuanceRole : itemIssuanceRoles) {
                        try {
                            delegator.removeValue(itemIssuanceRole);
                        } catch (Exception e) {
                            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                    }
                }
                try {
                    inventoryItemDetails = EntityQuery.use(delegator)
                            .from("InventoryItemDetail")
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying InventoryItemDetail: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (inventoryItemDetails != null) {
                    for (GenericValue inventoryItemDetail : inventoryItemDetails) {
                        inventoryItemDetail.put("itemIssuanceId", null);
                        try {
                            delegator.store(inventoryItemDetail);
                        } catch (Exception e) {
                            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                    }
                }
                try {
                    delegator.removeValue(itemIssuance);
                } catch (Exception e) {
                    Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }

        return "success";
    }

}

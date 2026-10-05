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
package com.ilscipio.scipio.manufacturing.event;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ModelService;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.workeffort.event.WorkEffortSimpleServices;

/**
 * SCIPIO: Hand-written replacement for the ProductionRunServices.xml simple-methods
 * (createProductionRunPartyAssign, createProductionRunAssoc, issueProductionRunTask,
 * issueProductionRunTaskComponent, issueInventoryItemToWorkEffort).
 */
public class ProductionRunSimpleServices {

    private static final String MODULE = ProductionRunSimpleServices.class.getName();
    private static final String RESOURCE = "ManufacturingUiLabels";

    /** Assigns the selected party to the production run or task. */
    public static Map<String, Object> createProductionRunPartyAssign(DispatchContext dctx, Map<String, ? extends Object> context) {
        Map<String, Object> parameters = UtilGenerics.cast(context);
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        parameters.put("statusId", "PRTYASGN_ASSIGNED");
        if (UtilValidate.isEmpty(parameters.get("workEffortId"))) {
            parameters.put("workEffortId", parameters.get("productionRunId"));
        }
        Map<String, Object> result = ServiceUtil.returnSuccess();
        result.put("productionRunId", parameters.get("productionRunId"));

        // SCIPIO: the legacy XML used <call-simple-method> (in-process call, same env/context)
        // rather than a full service invocation; calling the converted Java method directly
        // preserves that semantics.
        Map<String, Object> assignResult = WorkEffortSimpleServices.assignPartyToWorkEffort(dctx, parameters);
        if (ServiceUtil.isError(assignResult)) {
            return assignResult;
        }
        if (assignResult.get("fromDate") != null) {
            result.put("fromDate", assignResult.get("fromDate"));
        }
        return result;
    }

    /** Associates the production run to another production run. */
    public static Map<String, Object> createProductionRunAssoc(DispatchContext dctx, Map<String, ? extends Object> context) {
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Map<String, Object> parameters = UtilGenerics.cast(context);
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> ctx = new HashMap<>();
        String workFlowSequenceTypeId = (String) parameters.get("workFlowSequenceTypeId");
        if ("WF_PREDECESSOR".equals(workFlowSequenceTypeId)) {
            ctx.put("workEffortIdFrom", parameters.get("productionRunIdTo"));
            ctx.put("workEffortIdTo", parameters.get("productionRunId"));
        }
        if ("WF_SUCCESSOR".equals(workFlowSequenceTypeId)) {
            ctx.put("workEffortIdFrom", parameters.get("productionRunId"));
            ctx.put("workEffortIdTo", parameters.get("productionRunIdTo"));
        }
        ctx.put("workEffortAssocTypeId", "WORK_EFF_PRECEDENCY");
        try {
            ctx.put("userLogin", userLogin);
            Map<String, Object> serviceResult = dispatcher.runSync("createWorkEffortAssoc", ctx);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (GenericServiceException e) {
            Debug.logError(e, "Error calling createWorkEffortAssoc: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        return ServiceUtil.returnSuccess();
    }

    /** Issues the Inventory for a Production Run Task; skips the normal inventory reservation process. */
    public static Map<String, Object> issueProductionRunTask(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Map<String, Object> parameters = UtilGenerics.cast(context);
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        GenericValue workEffort;
        try {
            workEffort = EntityQuery.use(delegator).from("WorkEffort").where("workEffortId", parameters.get("workEffortId")).queryOne();
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error querying WorkEffort: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(parameters.get("failIfItemsAreNotAvailable"))) {
            parameters.put("failIfItemsAreNotAvailable", "Y");
        }
        if (UtilValidate.isEmpty(parameters.get("failIfItemsAreNotOnHand"))) {
            parameters.put("failIfItemsAreNotOnHand", "Y");
        }
        if (workEffort != null && !"PRUN_CANCELLED".equals(workEffort.getString("currentStatusId"))) {
            List<GenericValue> components;
            try {
                components = EntityQuery.use(delegator).from("WorkEffortGoodStandard")
                        .where("workEffortId", parameters.get("workEffortId"), "statusId", "WEGS_CREATED", "workEffortGoodStdTypeId", "PRUNT_PROD_NEEDED")
                        .filterByDate()
                        .queryList();
            } catch (GenericEntityException e) {
                Debug.logError(e, "Error querying WorkEffortGoodStandard: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            ModelService componentModel;
            try {
                componentModel = dctx.getModelService("issueProductionRunTaskComponent");
            } catch (GenericServiceException e) {
                Debug.logError(e, "Error getting model for issueProductionRunTaskComponent: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            for (GenericValue component : components) {
                if (UtilValidate.isNotEmpty(component.get("productId"))) {
                    // SCIPIO: mirrors <set-service-fields>; copies only fields matching IN attributes
                    // of issueProductionRunTaskComponent (workEffortId, productId, fromDate, description, ...)
                    Map<String, Object> callSvcMap = componentModel.makeValid(ModelService.IN_PARAM, component);
                    // SCIPIO: the legacy XML sets this from a bare "reserveOrderEnumId" env field that is
                    // never assigned anywhere in this method (only parameters.reserveOrderEnumId exists),
                    // so it is always null here; preserved as-is (issueProductionRunTaskComponent falls
                    // back to its own INVRO_FIFO_REC default when unset).
                    callSvcMap.put("description", "BOM Part");
                    callSvcMap.put("failIfItemsAreNotAvailable", parameters.get("failIfItemsAreNotAvailable"));
                    callSvcMap.put("failIfItemsAreNotOnHand", parameters.get("failIfItemsAreNotOnHand"));
                    try {
                        callSvcMap.put("userLogin", userLogin);
                        Map<String, Object> serviceResult = dispatcher.runSync("issueProductionRunTaskComponent", callSvcMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                        }
                    } catch (GenericServiceException e) {
                        Debug.logError(e, "Error calling issueProductionRunTaskComponent: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            }
            Debug.logInfo("Issued inventory for workEffortId " + workEffort.getString("workEffortId") + ".", MODULE);
        }
        return ServiceUtil.returnSuccess();
    }

    /**
     * Issues the Inventory for a Production Run Task Component. If fromDate is passed, the
     * WorkEffortGoodStandard record (workEffortId|productId|fromDate) with type PRUNT_PROD_NEEDED
     * supplies the quantity, and is marked COMPLETED once fully issued.
     */
    public static Map<String, Object> issueProductionRunTaskComponent(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        Map<String, Object> parameters = UtilGenerics.cast(context);
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        String productId;
        GenericValue workEffortGoodStandard = null;
        BigDecimal estimatedQuantity;
        if (UtilValidate.isEmpty(parameters.get("fromDate"))) {
            productId = (String) parameters.get("productId");
            estimatedQuantity = parameters.get("quantity") != null ? (BigDecimal) parameters.get("quantity") : BigDecimal.ZERO;
        } else {
            try {
                workEffortGoodStandard = EntityQuery.use(delegator).from("WorkEffortGoodStandard")
                        .where("workEffortId", parameters.get("workEffortId"),
                                "productId", parameters.get("productId"),
                                "fromDate", parameters.get("fromDate"),
                                "workEffortGoodStdTypeId", "PRUNT_PROD_NEEDED")
                        .queryOne();
            } catch (GenericEntityException e) {
                Debug.logError(e, "Error querying WorkEffortGoodStandard: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            productId = workEffortGoodStandard != null ? workEffortGoodStandard.getString("productId") : null;
            if (UtilValidate.isEmpty(parameters.get("quantity"))) {
                estimatedQuantity = workEffortGoodStandard != null ? workEffortGoodStandard.getBigDecimal("estimatedQuantity") : null;
            } else {
                estimatedQuantity = (BigDecimal) parameters.get("quantity");
            }
            if (estimatedQuantity == null) {
                estimatedQuantity = BigDecimal.ZERO;
            }
        }

        if (UtilValidate.isEmpty(productId)) {
            return ServiceUtil.returnSuccess();
        }

        GenericValue workEffort;
        try {
            workEffort = EntityQuery.use(delegator).from("WorkEffort").where("workEffortId", parameters.get("workEffortId")).queryOne();
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error querying WorkEffort: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        String orderByField;
        String reserveOrderEnumId = (String) parameters.get("reserveOrderEnumId");
        if ("INVRO_FIFO_EXP".equals(reserveOrderEnumId)) {
            orderByField = "+expireDate";
        } else if ("INVRO_LIFO_EXP".equals(reserveOrderEnumId)) {
            orderByField = "-expireDate";
        } else if ("INVRO_LIFO_REC".equals(reserveOrderEnumId)) {
            orderByField = "-datetimeReceived";
        } else {
            orderByField = "+datetimeReceived";
            parameters.put("reserveOrderEnumId", "INVRO_FIFO_REC");
        }

        Map<String, Object> lookupFieldMap = new HashMap<>();
        lookupFieldMap.put("productId", productId);
        lookupFieldMap.put("facilityId", workEffort != null ? workEffort.get("facilityId") : null);
        if (UtilValidate.isNotEmpty(parameters.get("lotId"))) {
            parameters.put("failIfItemsAreNotAvailable", "Y");
            lookupFieldMap.put("lotId", parameters.get("lotId"));
        }
        if (UtilValidate.isNotEmpty(parameters.get("locationSeqId"))) {
            lookupFieldMap.put("locationSeqId", parameters.get("locationSeqId"));
        }

        List<GenericValue> inventoryItemList;
        try {
            inventoryItemList = new LinkedList<>(EntityQuery.use(delegator).from("InventoryItem").where(lookupFieldMap).orderBy(orderByField).queryList());
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error querying InventoryItem: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(parameters.get("locationSeqId")) && UtilValidate.isNotEmpty(parameters.get("secondaryLocationSeqId"))) {
            lookupFieldMap.put("locationSeqId", parameters.get("secondaryLocationSeqId"));
            try {
                inventoryItemList.addAll(EntityQuery.use(delegator).from("InventoryItem").where(lookupFieldMap).orderBy(orderByField).queryList());
            } catch (GenericEntityException e) {
                Debug.logError(e, "Error querying InventoryItem (secondary location): " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        IssueTracker tracker = new IssueTracker();
        tracker.quantityNotIssued = estimatedQuantity;
        tracker.useReservedItems = false;

        for (GenericValue inventoryItem : inventoryItemList) {
            Map<String, Object> errorResult = issueProductionRunTaskComponentInline(delegator, dispatcher, parameters, tracker, inventoryItem);
            if (errorResult != null) {
                return errorResult;
            }
        }

        if (!"Y".equals(parameters.get("failIfItemsAreNotAvailable")) && tracker.quantityNotIssued.compareTo(BigDecimal.ZERO) > 0) {
            tracker.useReservedItems = true;
            for (GenericValue inventoryItem : inventoryItemList) {
                if (tracker.quantityNotIssued.compareTo(BigDecimal.ZERO) > 0) {
                    try {
                        inventoryItem.refresh();
                    } catch (GenericEntityException e) {
                        Debug.logError(e, "Error refreshing InventoryItem: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    Map<String, Object> errorResult = issueProductionRunTaskComponentInline(delegator, dispatcher, parameters, tracker, inventoryItem);
                    if (errorResult != null) {
                        return errorResult;
                    }
                }
            }
        }

        if (tracker.quantityNotIssued.compareTo(BigDecimal.ZERO) != 0) {
            if ("Y".equals(parameters.get("failIfItemsAreNotAvailable")) || UtilValidate.isEmpty(parameters.get("failIfItemsAreNotOnHand"))) {
                // SCIPIO: the label expects productId, internalName and parameters.quantityNotIssued
                String missingProductId = (String) parameters.get("productId");
                String missingName = missingProductId;
                try {
                    GenericValue missingProduct = EntityQuery.use(delegator).from("Product").where("productId", missingProductId).cache().queryOne();
                    if (missingProduct != null && UtilValidate.isNotEmpty(missingProduct.getString("internalName"))) {
                        missingName = missingProduct.getString("internalName");
                    }
                } catch (GenericEntityException e) {
                    Debug.logWarning(e, "Could not read product " + missingProductId, MODULE);
                }
                Map<String, Object> labelArgs = UtilMisc.toMap("productId", missingProductId, "internalName", missingName,
                        "parameters", UtilMisc.toMap("quantityNotIssued", tracker.quantityNotIssued));
                return ServiceUtil.returnError(UtilProperties.getMessage(RESOURCE, "ManufacturingMaterialsNotAvailable", labelArgs, locale));
            }
            if (tracker.lastNonSerInventoryItem != null) {
                Map<String, Object> issuanceCreateMap = new HashMap<>();
                issuanceCreateMap.put("workEffortId", parameters.get("workEffortId"));
                issuanceCreateMap.put("inventoryItemId", tracker.lastNonSerInventoryItem.getString("inventoryItemId"));
                issuanceCreateMap.put("quantity", tracker.quantityNotIssued);
                try {
                    issuanceCreateMap.put("userLogin", userLogin);
                    Map<String, Object> serviceResult = dispatcher.runSync("assignInventoryToWorkEffort", issuanceCreateMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                } catch (GenericServiceException e) {
                    Debug.logError(e, "Error calling assignInventoryToWorkEffort: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                Map<String, Object> createDetailMap = new HashMap<>();
                createDetailMap.put("inventoryItemId", tracker.lastNonSerInventoryItem.getString("inventoryItemId"));
                createDetailMap.put("workEffortId", parameters.get("workEffortId"));
                createDetailMap.put("availableToPromiseDiff", tracker.quantityNotIssued.negate());
                createDetailMap.put("quantityOnHandDiff", tracker.quantityNotIssued.negate());
                createDetailMap.put("reasonEnumId", parameters.get("reasonEnumId"));
                createDetailMap.put("description", parameters.get("description"));
                try {
                    createDetailMap.put("userLogin", userLogin);
                    Map<String, Object> serviceResult = dispatcher.runSync("createInventoryItemDetail", createDetailMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                } catch (GenericServiceException e) {
                    Debug.logError(e, "Error calling createInventoryItemDetail: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                Map<String, Object> balanceInventoryItemsInMap = new HashMap<>();
                balanceInventoryItemsInMap.put("inventoryItemId", tracker.lastNonSerInventoryItem.getString("inventoryItemId"));
                try {
                    balanceInventoryItemsInMap.put("userLogin", userLogin);
                    Map<String, Object> serviceResult = dispatcher.runSync("balanceInventoryItems", balanceInventoryItemsInMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                } catch (GenericServiceException e) {
                    Debug.logError(e, "Error calling balanceInventoryItems: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            } else {
                Map<String, Object> createInvItemInMap = new HashMap<>();
                createInvItemInMap.put("productId", productId);
                createInvItemInMap.put("facilityId", workEffort != null ? workEffort.get("facilityId") : null);
                createInvItemInMap.put("inventoryItemTypeId", "NON_SERIAL_INV_ITEM");
                String newInventoryItemId;
                try {
                    createInvItemInMap.put("userLogin", userLogin);
                    Map<String, Object> serviceResult = dispatcher.runSync("createInventoryItem", createInvItemInMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                    newInventoryItemId = (String) serviceResult.get("inventoryItemId");
                } catch (GenericServiceException e) {
                    Debug.logError(e, "Error calling createInventoryItem: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                Map<String, Object> issuanceCreateMap = new HashMap<>();
                issuanceCreateMap.put("workEffortId", parameters.get("workEffortId"));
                issuanceCreateMap.put("inventoryItemId", newInventoryItemId);
                issuanceCreateMap.put("quantity", tracker.quantityNotIssued);
                try {
                    issuanceCreateMap.put("userLogin", userLogin);
                    Map<String, Object> serviceResult = dispatcher.runSync("assignInventoryToWorkEffort", issuanceCreateMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                } catch (GenericServiceException e) {
                    Debug.logError(e, "Error calling assignInventoryToWorkEffort: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                Map<String, Object> createDetailMap = new HashMap<>();
                createDetailMap.put("inventoryItemId", newInventoryItemId);
                createDetailMap.put("workEffortId", parameters.get("workEffortId"));
                createDetailMap.put("availableToPromiseDiff", tracker.quantityNotIssued.negate());
                createDetailMap.put("quantityOnHandDiff", tracker.quantityNotIssued.negate());
                createDetailMap.put("reasonEnumId", parameters.get("reasonEnumId"));
                createDetailMap.put("description", parameters.get("description"));
                try {
                    createDetailMap.put("userLogin", userLogin);
                    Map<String, Object> serviceResult = dispatcher.runSync("createInventoryItemDetail", createDetailMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                } catch (GenericServiceException e) {
                    Debug.logError(e, "Error calling createInventoryItemDetail: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
            tracker.quantityNotIssued = BigDecimal.ZERO;
        }

        if (workEffortGoodStandard != null) {
            List<GenericValue> issuances;
            try {
                issuances = EntityQuery.use(delegator).from("WorkEffortAndInventoryAssign")
                        .where("workEffortId", workEffortGoodStandard.get("workEffortId"), "productId", workEffortGoodStandard.get("productId"))
                        .queryList();
            } catch (GenericEntityException e) {
                Debug.logError(e, "Error querying WorkEffortAndInventoryAssign: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            BigDecimal totalIssuance = BigDecimal.ZERO;
            for (GenericValue issuance : issuances) {
                BigDecimal quantity = issuance.getBigDecimal("quantity");
                if (quantity != null) {
                    totalIssuance = totalIssuance.add(quantity);
                }
            }
            BigDecimal estimated = workEffortGoodStandard.getBigDecimal("estimatedQuantity");
            if (estimated != null && estimated.compareTo(totalIssuance) <= 0) {
                workEffortGoodStandard.put("statusId", "WEGS_COMPLETED");
                try {
                    delegator.store(workEffortGoodStandard);
                } catch (GenericEntityException e) {
                    Debug.logError(e, "Error storing WorkEffortGoodStandard: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }

        return ServiceUtil.returnSuccess();
    }

    /** Mutable tracker shared across issueProductionRunTaskComponentInline calls for one issueProductionRunTaskComponent run. */
    private static final class IssueTracker {
        BigDecimal quantityNotIssued;
        boolean useReservedItems;
        GenericValue lastNonSerInventoryItem;
    }

    /** Does an issuance for one InventoryItem; mirrors the inline issueProductionRunTaskComponentInline simple-method. */
    private static Map<String, Object> issueProductionRunTaskComponentInline(Delegator delegator, LocalDispatcher dispatcher,
            Map<String, Object> parameters, IssueTracker tracker, GenericValue inventoryItem) {
        GenericValue userLogin = (GenericValue) parameters.get("userLogin");
        if (tracker.quantityNotIssued.compareTo(BigDecimal.ZERO) <= 0) {
            return null;
        }
        if ("SERIALIZED_INV_ITEM".equals(inventoryItem.getString("inventoryItemTypeId"))
                && "INV_AVAILABLE".equals(inventoryItem.getString("statusId"))) {
            inventoryItem.put("statusId", "INV_DELIVERED");
            try {
                delegator.store(inventoryItem);
            } catch (GenericEntityException e) {
                Debug.logError(e, "Error storing InventoryItem: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            Map<String, Object> issuanceCreateMap = new HashMap<>();
            issuanceCreateMap.put("workEffortId", parameters.get("workEffortId"));
            issuanceCreateMap.put("inventoryItemId", inventoryItem.getString("inventoryItemId"));
            issuanceCreateMap.put("quantity", BigDecimal.ONE);
            try {
                issuanceCreateMap.put("userLogin", userLogin);
                Map<String, Object> serviceResult = dispatcher.runSync("assignInventoryToWorkEffort", issuanceCreateMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (GenericServiceException e) {
                Debug.logError(e, "Error calling assignInventoryToWorkEffort: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            tracker.quantityNotIssued = tracker.quantityNotIssued.subtract(BigDecimal.ONE);
        }

        boolean eligible = (UtilValidate.isEmpty(inventoryItem.getString("statusId"))
                || "INV_AVAILABLE".equals(inventoryItem.getString("statusId"))
                || "INV_NS_RETURNED".equals(inventoryItem.getString("statusId")))
                && "NON_SERIAL_INV_ITEM".equals(inventoryItem.getString("inventoryItemTypeId"));
        if (eligible) {
            BigDecimal inventoryItemQuantity = tracker.useReservedItems
                    ? inventoryItem.getBigDecimal("quantityOnHandTotal")
                    : inventoryItem.getBigDecimal("availableToPromiseTotal");
            if (UtilValidate.isNotEmpty(inventoryItemQuantity) && inventoryItemQuantity.compareTo(BigDecimal.ZERO) > 0) {
                BigDecimal deductAmount = tracker.quantityNotIssued.compareTo(inventoryItemQuantity) > 0
                        ? inventoryItemQuantity : tracker.quantityNotIssued;

                Map<String, Object> issuanceCreateMap = new HashMap<>();
                issuanceCreateMap.put("workEffortId", parameters.get("workEffortId"));
                issuanceCreateMap.put("inventoryItemId", inventoryItem.getString("inventoryItemId"));
                issuanceCreateMap.put("quantity", deductAmount);
                try {
                    issuanceCreateMap.put("userLogin", userLogin);
                    Map<String, Object> serviceResult = dispatcher.runSync("assignInventoryToWorkEffort", issuanceCreateMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                } catch (GenericServiceException e) {
                    Debug.logError(e, "Error calling assignInventoryToWorkEffort: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }

                Map<String, Object> createDetailMap = new HashMap<>();
                createDetailMap.put("inventoryItemId", inventoryItem.getString("inventoryItemId"));
                createDetailMap.put("workEffortId", parameters.get("workEffortId"));
                createDetailMap.put("availableToPromiseDiff", deductAmount.negate());
                createDetailMap.put("quantityOnHandDiff", deductAmount.negate());
                createDetailMap.put("reasonEnumId", parameters.get("reasonEnumId"));
                createDetailMap.put("description", parameters.get("description"));
                try {
                    createDetailMap.put("userLogin", userLogin);
                    Map<String, Object> serviceResult = dispatcher.runSync("createInventoryItemDetail", createDetailMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                } catch (GenericServiceException e) {
                    Debug.logError(e, "Error calling createInventoryItemDetail: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }

                tracker.quantityNotIssued = tracker.quantityNotIssued.subtract(deductAmount);

                Map<String, Object> balanceInventoryItemsInMap = new HashMap<>();
                balanceInventoryItemsInMap.put("inventoryItemId", inventoryItem.getString("inventoryItemId"));
                try {
                    balanceInventoryItemsInMap.put("userLogin", userLogin);
                    Map<String, Object> serviceResult = dispatcher.runSync("balanceInventoryItems", balanceInventoryItemsInMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                } catch (GenericServiceException e) {
                    Debug.logError(e, "Error calling balanceInventoryItems: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
            tracker.lastNonSerInventoryItem = inventoryItem;
        }
        return null;
    }

    /** Issues one InventoryItem (or part of it) to a WorkEffort; skips the normal inventory reservation process. */
    public static Map<String, Object> issueInventoryItemToWorkEffort(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Map<String, Object> parameters = UtilGenerics.cast(context);
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        GenericValue inventoryItem = (GenericValue) parameters.get("inventoryItem");
        Map<String, Object> result = ServiceUtil.returnSuccess();
        result.put("finishedProductId", inventoryItem.get("productId"));

        if ("SERIALIZED_INV_ITEM".equals(inventoryItem.getString("inventoryItemTypeId"))
                && "INV_AVAILABLE".equals(inventoryItem.getString("statusId"))) {
            inventoryItem.put("statusId", "INV_DELIVERED"); // SCIPIO: in-memory only, matches legacy XML (never persisted here)
            // SCIPIO: the legacy XML calls updateInventoryItem with an "updateContext" map that is never
            // populated anywhere in this method (pre-existing bug in the live XML); preserved as-is, so
            // this call will fail (missing required inventoryItemId) whenever this branch is taken.
            Map<String, Object> updateContext = new HashMap<>();
            try {
                updateContext.put("userLogin", userLogin);
                Map<String, Object> serviceResult = dispatcher.runSync("updateInventoryItem", updateContext);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (GenericServiceException e) {
                Debug.logError(e, "Error calling updateInventoryItem: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            Map<String, Object> issuanceCreateMap = new HashMap<>();
            issuanceCreateMap.put("workEffortId", parameters.get("workEffortId"));
            issuanceCreateMap.put("inventoryItemId", inventoryItem.getString("inventoryItemId"));
            issuanceCreateMap.put("quantity", BigDecimal.ONE);
            try {
                issuanceCreateMap.put("userLogin", userLogin);
                Map<String, Object> serviceResult = dispatcher.runSync("assignInventoryToWorkEffort", issuanceCreateMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (GenericServiceException e) {
                Debug.logError(e, "Error calling assignInventoryToWorkEffort: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            result.put("quantityIssued", issuanceCreateMap.get("quantity"));
        }

        // SCIPIO: this second block is unconditional (not an "else" of the block above) in the legacy
        // XML, so for a SERIALIZED_INV_ITEM it always runs too and its else branch overwrites
        // quantityIssued back to 0; preserved as-is.
        BigDecimal availableToPromiseTotal = inventoryItem.getBigDecimal("availableToPromiseTotal");
        if ("NON_SERIAL_INV_ITEM".equals(inventoryItem.getString("inventoryItemTypeId"))
                && UtilValidate.isNotEmpty(availableToPromiseTotal)
                && availableToPromiseTotal.compareTo(BigDecimal.ZERO) > 0) {
            BigDecimal quantity = (BigDecimal) parameters.get("quantity");
            BigDecimal deductAmount = (UtilValidate.isEmpty(quantity) || quantity.compareTo(availableToPromiseTotal) > 0)
                    ? availableToPromiseTotal : quantity;

            Map<String, Object> issuanceCreateMap = new HashMap<>();
            issuanceCreateMap.put("workEffortId", parameters.get("workEffortId"));
            issuanceCreateMap.put("inventoryItemId", inventoryItem.getString("inventoryItemId"));
            issuanceCreateMap.put("quantity", deductAmount);
            try {
                issuanceCreateMap.put("userLogin", userLogin);
                Map<String, Object> serviceResult = dispatcher.runSync("assignInventoryToWorkEffort", issuanceCreateMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (GenericServiceException e) {
                Debug.logError(e, "Error calling assignInventoryToWorkEffort: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }

            Map<String, Object> createDetailMap = new HashMap<>();
            createDetailMap.put("inventoryItemId", inventoryItem.getString("inventoryItemId"));
            createDetailMap.put("workEffortId", parameters.get("workEffortId"));
            createDetailMap.put("availableToPromiseDiff", deductAmount.negate());
            createDetailMap.put("quantityOnHandDiff", deductAmount.negate());
            try {
                createDetailMap.put("userLogin", userLogin);
                Map<String, Object> serviceResult = dispatcher.runSync("createInventoryItemDetail", createDetailMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (GenericServiceException e) {
                Debug.logError(e, "Error calling createInventoryItemDetail: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            result.put("quantityIssued", deductAmount);
        } else {
            result.put("quantityIssued", BigDecimal.ZERO);
        }

        return result;
    }

    /**
     * Replaces a production run task component (WorkEffortGoodStandard, type PRUNT_PROD_NEEDED) with a
     * different product. Rejects the request when the component being replaced was already issued (a
     * WorkEffortInventoryAssign row exists for the task and productId); otherwise expires the old
     * WorkEffortGoodStandard row and creates a new one for newProductId, carrying over the same
     * estimatedQuantity unless a quantity is supplied.
     */
    public static Map<String, Object> replaceProductionRunComponent(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        Locale locale = (Locale) context.get("locale");
        Map<String, Object> parameters = UtilGenerics.cast(context);

        String productionRunId = (String) parameters.get("productionRunId");
        String workEffortId = (String) parameters.get("workEffortId");
        String productId = (String) parameters.get("productId");
        String newProductId = (String) parameters.get("newProductId");
        BigDecimal quantity = (BigDecimal) parameters.get("quantity");

        List<GenericValue> assigns;
        try {
            assigns = EntityQuery.use(delegator).from("WorkEffortAndInventoryAssign")
                    .where("workEffortId", workEffortId, "productId", productId)
                    .queryList();
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error querying WorkEffortAndInventoryAssign: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(assigns)) {
            return ServiceUtil.returnError(UtilProperties.getMessage(RESOURCE, "ManufacturingComponentAlreadyIssued", locale));
        }

        GenericValue oldComponent;
        try {
            oldComponent = EntityQuery.use(delegator).from("WorkEffortGoodStandard")
                    .where("workEffortId", workEffortId, "productId", productId, "workEffortGoodStdTypeId", "PRUNT_PROD_NEEDED")
                    .filterByDate()
                    .queryFirst();
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error querying WorkEffortGoodStandard: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (oldComponent == null) {
            return ServiceUtil.returnError(UtilProperties.getMessage(RESOURCE, "ManufacturingComponentNotFound", locale));
        }

        BigDecimal estimatedQuantity = quantity != null ? quantity : oldComponent.getBigDecimal("estimatedQuantity");
        Timestamp now = UtilDateTime.nowTimestamp();
        try {
            oldComponent.set("thruDate", now);
            oldComponent.store();

            GenericValue newComponent = delegator.makeValue("WorkEffortGoodStandard");
            newComponent.set("workEffortId", workEffortId);
            newComponent.set("productId", newProductId);
            newComponent.set("workEffortGoodStdTypeId", "PRUNT_PROD_NEEDED");
            newComponent.set("fromDate", now);
            newComponent.set("statusId", oldComponent.getString("statusId"));
            newComponent.set("estimatedQuantity", estimatedQuantity);
            delegator.create(newComponent);
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error replacing production run component: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        Map<String, Object> result = ServiceUtil.returnSuccess();
        result.put("productionRunId", productionRunId);
        return result;
    }
}

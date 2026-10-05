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
package com.ilscipio.scipio.manufacturing.reservation;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Lot reservation services: hold a whole inventory lot for a production run task by pushing its
 * available-to-promise down (quantity on hand stays untouched), release it back, and keep the reservation
 * in step when the task later issues material from the reserved item.
 *
 * <p>SCIPIO: 4.0.0: New.</p>
 */
public class ReservationServices {

    private static final String MODULE = ReservationServices.class.getName();
    private static final String RESOURCE = "ManufacturingUiLabels";

    private ReservationServices() {}

    /** Holds a lot for a production run task by reserving the InventoryItem's whole available-to-promise. */
    public static Map<String, Object> reserveProductionRunLot(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        Map<String, Object> parameters = UtilGenerics.cast(context);

        if (!dctx.getSecurity().hasEntityPermission("MANUFACTURING", "_UPDATE", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage(RESOURCE, "ManufacturingUpdatePermissionError", locale));
        }

        String workEffortId = (String) parameters.get("workEffortId");
        String productionRunId = (String) parameters.get("productionRunId");
        String productId = (String) parameters.get("productId");
        String lotId = (String) parameters.get("lotId");
        String inventoryItemId = (String) parameters.get("inventoryItemId");

        GenericValue task;
        GenericValue inventoryItem;
        try {
            task = EntityQuery.use(delegator).from("WorkEffort").where("workEffortId", workEffortId).queryOne();
            if (task == null) {
                return ServiceUtil.returnError(UtilProperties.getMessage(RESOURCE, "ManufacturingProductionRunTaskNotFound",
                        UtilMisc.toMap("productionRunTaskId", workEffortId), locale));
            }
            if (UtilValidate.isEmpty(productionRunId)) {
                productionRunId = task.getString("workEffortParentId");
            } else if (!productionRunId.equals(task.getString("workEffortParentId"))) {
                return ServiceUtil.returnError(UtilProperties.getMessage(RESOURCE, "ManufacturingProductionRunTaskNotFound",
                        UtilMisc.toMap("productionRunTaskId", workEffortId), locale));
            }

            GenericValue component = EntityQuery.use(delegator).from("WorkEffortGoodStandard")
                    .where("workEffortId", workEffortId, "productId", productId, "workEffortGoodStdTypeId", "PRUNT_PROD_NEEDED")
                    .filterByDate().queryFirst();
            if (component == null) {
                return ServiceUtil.returnError(UtilProperties.getMessage(RESOURCE, "ManufacturingProductionRunLotComponentNotFound", locale));
            }

            GenericValue productionRun = EntityQuery.use(delegator).from("WorkEffort").where("workEffortId", productionRunId).queryOne();
            String facilityId = productionRun != null ? productionRun.getString("facilityId") : null;

            if (UtilValidate.isNotEmpty(inventoryItemId)) {
                inventoryItem = EntityQuery.use(delegator).from("InventoryItem").where("inventoryItemId", inventoryItemId).queryOne();
            } else if (UtilValidate.isNotEmpty(lotId)) {
                inventoryItem = EntityQuery.use(delegator).from("InventoryItem")
                        .where("productId", productId, "lotId", lotId, "facilityId", facilityId).queryFirst();
            } else {
                return ServiceUtil.returnError(UtilProperties.getMessage(RESOURCE, "ManufacturingProductionRunLotNotFound", locale));
            }
            if (inventoryItem == null || !productId.equals(inventoryItem.getString("productId"))) {
                return ServiceUtil.returnError(UtilProperties.getMessage(RESOURCE, "ManufacturingProductionRunLotNotFound", locale));
            }
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error looking up task/component/inventory item: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        BigDecimal atp = inventoryItem.getBigDecimal("availableToPromiseTotal");
        if (atp == null || atp.compareTo(BigDecimal.ZERO) <= 0) {
            return ServiceUtil.returnError(UtilProperties.getMessage(RESOURCE, "ManufacturingProductionRunLotNoAvailableToPromise", locale));
        }
        String resolvedInventoryItemId = inventoryItem.getString("inventoryItemId");

        try {
            GenericValue existing = EntityQuery.use(delegator).from("ProductionRunLotReservation")
                    .where("workEffortId", workEffortId, "inventoryItemId", resolvedInventoryItemId).queryOne();
            if (existing != null && existing.get("releasedDate") == null) {
                return ServiceUtil.returnError(UtilProperties.getMessage(RESOURCE, "ManufacturingProductionRunLotAlreadyReserved", locale));
            }

            Map<String, Object> createDetailMap = new HashMap<>();
            createDetailMap.put("inventoryItemId", resolvedInventoryItemId);
            createDetailMap.put("workEffortId", workEffortId);
            createDetailMap.put("availableToPromiseDiff", atp.negate());
            createDetailMap.put("userLogin", userLogin);
            Map<String, Object> detailResult = dispatcher.runSync("createInventoryItemDetail", createDetailMap);
            if (ServiceUtil.isError(detailResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(detailResult));
            }

            Timestamp now = UtilDateTime.nowTimestamp();
            GenericValue product = EntityQuery.use(delegator).from("Product").where("productId", productId).queryOne();
            String quantityUomId = product != null ? product.getString("quantityUomId") : null;

            GenericValue reservation = (existing != null) ? existing : delegator.makeValue("ProductionRunLotReservation",
                    UtilMisc.toMap("workEffortId", workEffortId, "inventoryItemId", resolvedInventoryItemId));
            reservation.set("productionRunId", productionRunId);
            reservation.set("productId", productId);
            reservation.set("lotId", inventoryItem.getString("lotId"));
            reservation.set("facilityId", inventoryItem.getString("facilityId"));
            reservation.set("quantityReserved", atp);
            reservation.set("quantityUomId", quantityUomId);
            reservation.set("reservedDate", now);
            reservation.set("releasedDate", null);
            reservation.set("userLoginId", userLogin != null ? userLogin.getString("userLoginId") : null);
            if (existing != null) {
                reservation.store();
            } else {
                delegator.create(reservation);
            }

            Map<String, Object> result = ServiceUtil.returnSuccess();
            result.put("inventoryItemId", resolvedInventoryItemId);
            result.put("quantityReserved", atp);
            return result;
        } catch (GenericEntityException | GenericServiceException e) {
            Debug.logError(e, "Error reserving production run lot: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
    }

    /** Releases one open lot reservation: gives the reserved quantity back to available-to-promise and marks it released. */
    public static Map<String, Object> releaseProductionRunLot(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        Map<String, Object> parameters = UtilGenerics.cast(context);

        if (!dctx.getSecurity().hasEntityPermission("MANUFACTURING", "_UPDATE", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage(RESOURCE, "ManufacturingUpdatePermissionError", locale));
        }

        String workEffortId = (String) parameters.get("workEffortId");
        String inventoryItemId = (String) parameters.get("inventoryItemId");

        try {
            GenericValue reservation = EntityQuery.use(delegator).from("ProductionRunLotReservation")
                    .where("workEffortId", workEffortId, "inventoryItemId", inventoryItemId).queryOne();
            if (reservation == null || reservation.get("releasedDate") != null) {
                return ServiceUtil.returnError(UtilProperties.getMessage(RESOURCE, "ManufacturingProductionRunReservationNotFound", locale));
            }

            BigDecimal quantityReserved = reservation.getBigDecimal("quantityReserved");
            if (quantityReserved != null && quantityReserved.compareTo(BigDecimal.ZERO) > 0) {
                Map<String, Object> createDetailMap = new HashMap<>();
                createDetailMap.put("inventoryItemId", inventoryItemId);
                createDetailMap.put("workEffortId", workEffortId);
                createDetailMap.put("availableToPromiseDiff", quantityReserved);
                createDetailMap.put("userLogin", userLogin);
                Map<String, Object> detailResult = dispatcher.runSync("createInventoryItemDetail", createDetailMap);
                if (ServiceUtil.isError(detailResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(detailResult));
                }
            }

            reservation.set("releasedDate", UtilDateTime.nowTimestamp());
            reservation.store();
            return ServiceUtil.returnSuccess();
        } catch (GenericEntityException | GenericServiceException e) {
            Debug.logError(e, "Error releasing production run lot reservation: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
    }

    /** Releases every open lot reservation of every task of a production run; called when the run closes. */
    public static Map<String, Object> releaseProductionRunReservations(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        Map<String, Object> parameters = UtilGenerics.cast(context);

        String productionRunId = (String) parameters.get("productionRunId");
        int releasedCount = 0;
        try {
            List<GenericValue> openReservations = EntityQuery.use(delegator).from("ProductionRunLotReservation")
                    .where(EntityCondition.makeCondition(
                            EntityCondition.makeCondition("productionRunId", productionRunId),
                            EntityCondition.makeCondition("releasedDate", EntityOperator.EQUALS, null)))
                    .queryList();
            for (GenericValue reservation : openReservations) {
                Map<String, Object> releaseCtx = new HashMap<>();
                releaseCtx.put("workEffortId", reservation.getString("workEffortId"));
                releaseCtx.put("inventoryItemId", reservation.getString("inventoryItemId"));
                releaseCtx.put("userLogin", userLogin);
                try {
                    Map<String, Object> releaseResult = dispatcher.runSync("releaseProductionRunLot", releaseCtx);
                    if (ServiceUtil.isError(releaseResult)) {
                        Debug.logError("Error releasing reservation " + reservation.getPrimaryKey() + ": "
                                + ServiceUtil.getErrorMessage(releaseResult), MODULE);
                        continue;
                    }
                    releasedCount++;
                } catch (GenericServiceException e) {
                    Debug.logError(e, "Error calling releaseProductionRunLot for " + reservation.getPrimaryKey() + ": " + e.getMessage(), MODULE);
                }
            }
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error looking up open reservations for production run " + productionRunId + ": " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        Map<String, Object> result = ServiceUtil.returnSuccess();
        result.put("releasedCount", releasedCount);
        return result;
    }

    /**
     * Gives back to available-to-promise the quantity just issued from a reserved inventory item, and lowers
     * the reservation's held quantity by the same amount (fully releasing it once exhausted). A no-op when
     * the item carries no open reservation for the task, which is the common case since most issuances are
     * not against a reserved lot.
     */
    public static Map<String, Object> adjustProductionRunLotReservation(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        Map<String, Object> parameters = UtilGenerics.cast(context);

        String workEffortId = (String) parameters.get("workEffortId");
        String inventoryItemId = (String) parameters.get("inventoryItemId");
        if (UtilValidate.isEmpty(inventoryItemId)) {
            GenericValue inventoryItem = (GenericValue) parameters.get("inventoryItem");
            if (inventoryItem != null) {
                inventoryItemId = inventoryItem.getString("inventoryItemId");
            }
        }
        BigDecimal quantity = (BigDecimal) parameters.get("quantity");
        if (quantity == null) {
            quantity = (BigDecimal) parameters.get("quantityIssued");
        }
        if (UtilValidate.isEmpty(workEffortId) || UtilValidate.isEmpty(inventoryItemId)
                || quantity == null || quantity.compareTo(BigDecimal.ZERO) <= 0) {
            return ServiceUtil.returnSuccess();
        }

        try {
            GenericValue reservation = EntityQuery.use(delegator).from("ProductionRunLotReservation")
                    .where("workEffortId", workEffortId, "inventoryItemId", inventoryItemId).queryOne();
            if (reservation == null || reservation.get("releasedDate") != null) {
                // SCIPIO: not a reserved item (or already released); the issuance's own ATP deduction stands as-is
                return ServiceUtil.returnSuccess();
            }

            BigDecimal quantityReserved = reservation.getBigDecimal("quantityReserved");
            if (quantityReserved == null) {
                quantityReserved = BigDecimal.ZERO;
            }
            BigDecimal giveBack = (quantity.compareTo(quantityReserved) > 0) ? quantityReserved : quantity;
            if (giveBack.compareTo(BigDecimal.ZERO) > 0) {
                Map<String, Object> createDetailMap = new HashMap<>();
                createDetailMap.put("inventoryItemId", inventoryItemId);
                createDetailMap.put("workEffortId", workEffortId);
                createDetailMap.put("availableToPromiseDiff", giveBack);
                createDetailMap.put("userLogin", userLogin);
                Map<String, Object> detailResult = dispatcher.runSync("createInventoryItemDetail", createDetailMap);
                if (ServiceUtil.isError(detailResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(detailResult));
                }
            }

            BigDecimal remaining = quantityReserved.subtract(giveBack);
            reservation.set("quantityReserved", remaining);
            if (remaining.compareTo(BigDecimal.ZERO) <= 0) {
                reservation.set("releasedDate", UtilDateTime.nowTimestamp());
            }
            reservation.store();
            return ServiceUtil.returnSuccess();
        } catch (GenericEntityException | GenericServiceException e) {
            Debug.logError(e, "Error adjusting production run lot reservation: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
    }

    /** Returns the open lot reservations of a production run's tasks, enriched for display. */
    public static Map<String, Object> getProductionRunReservations(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        Map<String, Object> parameters = UtilGenerics.cast(context);

        if (!dctx.getSecurity().hasEntityPermission("MANUFACTURING", "_VIEW", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage(RESOURCE, "ManufacturingViewPermissionError", locale));
        }

        String productionRunId = (String) parameters.get("productionRunId");
        List<Map<String, Object>> reservations = new ArrayList<>();
        if (UtilValidate.isEmpty(productionRunId)) {
            Map<String, Object> empty = ServiceUtil.returnSuccess();
            empty.put("reservations", reservations);
            return empty;
        }

        try {
            List<GenericValue> openReservations = EntityQuery.use(delegator).from("ProductionRunLotReservation")
                    .where(EntityCondition.makeCondition(
                            EntityCondition.makeCondition("productionRunId", productionRunId),
                            EntityCondition.makeCondition("releasedDate", EntityOperator.EQUALS, null)))
                    .orderBy("reservedDate").queryList();
            for (GenericValue reservation : openReservations) {
                Map<String, Object> row = new HashMap<>(reservation.getAllFields());
                GenericValue task = EntityQuery.use(delegator).from("WorkEffort")
                        .where("workEffortId", reservation.getString("workEffortId")).queryOne();
                row.put("taskName", task != null ? task.get("workEffortName") : null);
                GenericValue product = EntityQuery.use(delegator).from("Product")
                        .where("productId", reservation.getString("productId")).queryOne();
                row.put("internalName", product != null ? product.get("internalName") : null);
                String quantityUomId = reservation.getString("quantityUomId");
                if (UtilValidate.isNotEmpty(quantityUomId)) {
                    GenericValue uom = EntityQuery.use(delegator).from("Uom").where("uomId", quantityUomId).queryOne();
                    row.put("uomAbbreviation", uom != null ? uom.get("abbreviation") : quantityUomId);
                }
                reservations.add(row);
            }
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error looking up open reservations for production run " + productionRunId + ": " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        Map<String, Object> result = ServiceUtil.returnSuccess();
        result.put("reservations", reservations);
        return result;
    }

}

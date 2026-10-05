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
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GroovyUtil;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.product.product.ProductWorker;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class ShipmentServices {

    private static final String MODULE = ShipmentServices.class.getName();


    /**
     * Create Shipment
     */
    public static Map<String, Object> createShipment(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> assignPartyToWorkEffortShip = null;
        GenericValue newEntity = null;
        Map<String, Object> shipWorkEffortMap = null;
        Map<String, Object> assignPartyToWorkEffortArrival = null;
        Map<String, Object> arrivalWorkEffortMap = null;
        GenericValue newStatusValue = null;
        newEntity = delegator.makeValue("Shipment");
        newEntity.setNonPKFields(context);
        if (UtilValidate.isNotEmpty(context.get("shipmentId"))) {
            newEntity.setPKFields(context);
        } else {
            ((GenericValue) newEntity).put("shipmentId", delegator.getNextSeqId("Shipment"));
        }
        result.put("shipmentId", newEntity.get("shipmentId"));
        Timestamp newEntity_createdDate = new Timestamp(System.currentTimeMillis());
        newEntity.put("createdByUserLogin", userLogin.get("userLoginId"));
        Timestamp newEntity_lastModifiedDate = new Timestamp(System.currentTimeMillis());
        newEntity.put("lastModifiedByUserLogin", userLogin.get("userLoginId"));
        if (UtilValidate.isNotEmpty(context.get("estimatedShipDate"))) {
            shipWorkEffortMap.put("workEffortName", "Shipment #" + newEntity.get("shipmentId") + " " + newEntity.get("primaryOrderId") + " Ship");
            shipWorkEffortMap.put("workEffortTypeId", "EVENT");
            shipWorkEffortMap.put("currentStatusId", "CAL_TENTATIVE");
            shipWorkEffortMap.put("estimatedStartDate", context.get("estimatedShipDate"));
            shipWorkEffortMap.put("estimatedCompletionDate", context.get("estimatedShipDate"));
            shipWorkEffortMap.put("facilityId", context.get("originFacilityId"));
            shipWorkEffortMap.put("quickAssignPartyId", userLogin.get("partyId"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createWorkEffort", shipWorkEffortMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
                newEntity.put("estimatedShipWorkEffId", serviceResult.get("workEffortId"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling createWorkEffort: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (UtilValidate.isNotEmpty(newEntity.get("partyIdFrom"))) {
                assignPartyToWorkEffortShip.put("workEffortId", newEntity.get("estimatedShipWorkEffId"));
                assignPartyToWorkEffortShip.put("partyId", newEntity.get("partyIdFrom"));
                assignPartyToWorkEffortShip.put("roleTypeId", "CAL_ATTENDEE");
                assignPartyToWorkEffortShip.put("statusId", "CAL_SENT");
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("assignPartyToWorkEffort", assignPartyToWorkEffortShip);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling assignPartyToWorkEffort: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }
        if (UtilValidate.isNotEmpty(context.get("estimatedArrivalDate"))) {
            arrivalWorkEffortMap.put("workEffortName", "Shipment #" + newEntity.get("shipmentId") + " " + newEntity.get("primaryOrderId") + " Arrival");
            arrivalWorkEffortMap.put("workEffortTypeId", "EVENT");
            arrivalWorkEffortMap.put("currentStatusId", "CAL_TENTATIVE");
            arrivalWorkEffortMap.put("estimatedStartDate", context.get("estimatedArrivalDate"));
            arrivalWorkEffortMap.put("estimatedCompletionDate", context.get("estimatedArrivalDate"));
            arrivalWorkEffortMap.put("facilityId", context.get("destinationFacilityId"));
            arrivalWorkEffortMap.put("quickAssignPartyId", userLogin.get("partyId"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createWorkEffort", arrivalWorkEffortMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
                newEntity.put("estimatedArrivalWorkEffId", serviceResult.get("workEffortId"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling createWorkEffort: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (UtilValidate.isNotEmpty(newEntity.get("partyIdTo"))) {
                assignPartyToWorkEffortArrival.put("workEffortId", newEntity.get("estimatedArrivalWorkEffId"));
                assignPartyToWorkEffortArrival.put("partyId", newEntity.get("partyIdTo"));
                assignPartyToWorkEffortArrival.put("roleTypeId", "CAL_ATTENDEE");
                assignPartyToWorkEffortArrival.put("statusId", "CAL_SENT");
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("assignPartyToWorkEffort", assignPartyToWorkEffortArrival);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling assignPartyToWorkEffort: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(newEntity.get("statusId"))) {
            newStatusValue = delegator.makeValue("ShipmentStatus");
            newStatusValue.put("statusId", newEntity.get("statusId"));
            newStatusValue.put("shipmentId", newEntity.get("shipmentId"));
            Timestamp newStatusValue_statusDate = new Timestamp(System.currentTimeMillis());
            try {
                delegator.create(newStatusValue);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            result.put("statusId", newEntity.get("statusId"));
        }
        result.put("shipmentTypeId", newEntity.get("shipmentTypeId"));

        return result;
    }


    /**
     * Update Shipment
     */
    public static Map<String, Object> updateShipment(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue newStatusValue = null;
        GenericValue checkStatusValidChange = null;
        GenericValue shipmentStatus = null;
        Map<String, Object> estShipWeUpdMap = null;
        GenericValue estShipWe = null;
        GenericValue estimatedArrivalWorkEffort = null;
        Map<String, Object> estimatedArrivalWorkEffortUpdMap = null;
        Map<String, Object> assignPartyToWorkEffortShip = null;
        List<GenericValue> emptyField = null;
        List<GenericValue> existingShipWepas = null;
        Map<String, Object> assignPartyToWorkEffortArrival = null;
        List<GenericValue> existingArrivalWepas = null;
        Object fromStatusId = null;
        Map<String, Object> inlineResult = null;
        Object operationName = "Update Shipment";
        inlineResult = checkCanChangeShipmentStatusDelivered(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue lookupPKMap = delegator.makeValue("Shipment");
        lookupPKMap.setPKFields(context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from(lookupPKMap.getEntityName())
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key : " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        result.put("shipmentTypeId", lookedUpValue.get("shipmentTypeId"));
        if (UtilValidate.isNotEmpty(context.get("statusId"))) {
            if (!java.util.Objects.equals(context.get("statusId"), lookedUpValue.get("statusId"))) {
                try {
                    checkStatusValidChange = EntityQuery.use(delegator)
                            .from("StatusValidChange")
                            .where(UtilMisc.toMap("statusId", lookedUpValue.get("statusId"), "statusIdTo", context.get("statusId")))
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying StatusValidChange: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (UtilValidate.isEmpty(checkStatusValidChange)) {
                    error_list.add("ERROR: Changing the status from " + lookedUpValue.get("statusId") + " to " + context.get("statusId") + " is not allowed.");
                }
                try {
                    shipmentStatus = EntityQuery.use(delegator)
                            .from("ShipmentStatus")
                            .where(context)
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying ShipmentStatus: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (UtilValidate.isEmpty(shipmentStatus)) {
                    newStatusValue = delegator.makeValue("ShipmentStatus");
                    newStatusValue.put("statusId", context.get("statusId"));
                    newStatusValue.put("shipmentId", context.get("shipmentId"));
                    Timestamp newStatusValue_statusDate = new Timestamp(System.currentTimeMillis());
                    if (UtilValidate.isEmpty(context.get("eventDate"))) {
                        newStatusValue_statusDate = new Timestamp(System.currentTimeMillis());
                    } else {
                        newStatusValue.put("statusDate", context.get("eventDate"));
                    }
                    try {
                        delegator.create(newStatusValue);
                    } catch (Exception e) {
                        Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                } else {
                    if (UtilValidate.isEmpty(context.get("eventDate"))) {
                        Timestamp shipmentStatus_statusDate = new Timestamp(System.currentTimeMillis());
                    } else {
                        shipmentStatus.put("statusDate", context.get("eventDate"));
                    }
                    try {
                        delegator.store(shipmentStatus);
                    } catch (Exception e) {
                        Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            }
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        Object estShipWe_estimatedStartDate = null;
        Object estShipWe_estimatedCompletionDate = null;
        Object estShipWe_facilityId = null;
        Object estShipWe_currentStatusId = null;
        if (((!(UtilValidate.isEmpty(context.get("estimatedShipDate"))) && !java.util.Objects.equals(context.get("estimatedShipDate"), lookedUpValue.get("estimatedShipDate"))) || (!(UtilValidate.isEmpty(context.get("originFacilityId"))) && !java.util.Objects.equals(context.get("originFacilityId"), lookedUpValue.get("originFacilityId"))) || (!(UtilValidate.isEmpty(context.get("statusId"))) && !java.util.Objects.equals(context.get("statusId"), lookedUpValue.get("statusId")) && ("SHIPMENT_CANCELLED".equals(context.get("statusId")) || "SHIPMENT_PACKED".equals(context.get("statusId")) || "SHIPMENT_SHIPPED".equals(context.get("statusId")))))) {
            try {
                estShipWe = EntityQuery.use(delegator)
                        .from("WorkEffort")
                        .where(UtilMisc.toMap("workEffortId", lookedUpValue.get("estimatedShipWorkEffId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying WorkEffort: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (UtilValidate.isNotEmpty(estShipWe)) {
                estShipWe.put("estimatedStartDate", context.get("estimatedShipDate"));
                estShipWe.put("estimatedCompletionDate", context.get("estimatedShipDate"));
                estShipWe.put("facilityId", context.get("originFacilityId"));
                if ((!(UtilValidate.isEmpty(context.get("statusId"))) && !java.util.Objects.equals(context.get("statusId"), lookedUpValue.get("statusId")))) {
                    if ("SHIPMENT_CANCELLED".equals(context.get("statusId"))) {
                        estShipWe.put("currentStatusId", "CAL_CANCELLED");
                    }
                    if ("SHIPMENT_PACKED".equals(context.get("statusId"))) {
                        estShipWe.put("currentStatusId", "CAL_CONFIRMED");
                    }
                    if ("SHIPMENT_SHIPPED".equals(context.get("statusId"))) {
                        estShipWe.put("currentStatusId", "CAL_COMPLETED");
                    }
                }
                // set-service-fields from "estShipWe" to "estShipWeUpdMap" for service "updateWorkEffort"
                estShipWeUpdMap.putAll(UtilMisc.toMap(estShipWe));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("updateWorkEffort", estShipWeUpdMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling updateWorkEffort: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }
        Object estimatedArrivalWorkEffort_estimatedStartDate = null;
        Object estimatedArrivalWorkEffort_estimatedCompletionDate = null;
        Object estimatedArrivalWorkEffort_facilityId = null;
        if (((!(UtilValidate.isEmpty(context.get("estimatedArrivalDate"))) && !java.util.Objects.equals(context.get("estimatedArrivalDate"), lookedUpValue.get("estimatedArrivalDate"))) || (!(UtilValidate.isEmpty(context.get("destinationFacilityId"))) && !java.util.Objects.equals(context.get("destinationFacilityId"), lookedUpValue.get("destinationFacilityId"))))) {
            try {
                estimatedArrivalWorkEffort = EntityQuery.use(delegator)
                        .from("WorkEffort")
                        .where(UtilMisc.toMap("workEffortId", lookedUpValue.get("estimatedArrivalWorkEffId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying WorkEffort: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (UtilValidate.isNotEmpty(estimatedArrivalWorkEffort)) {
                estimatedArrivalWorkEffort.put("estimatedStartDate", context.get("estimatedArrivalDate"));
                estimatedArrivalWorkEffort.put("estimatedCompletionDate", context.get("estimatedArrivalDate"));
                estimatedArrivalWorkEffort.put("facilityId", context.get("destinationFacilityId"));
                // set-service-fields from "estimatedArrivalWorkEffort" to "estimatedArrivalWorkEffortUpdMap" for service "updateWorkEffort"
                estimatedArrivalWorkEffortUpdMap.putAll(UtilMisc.toMap(estimatedArrivalWorkEffort));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("updateWorkEffort", estimatedArrivalWorkEffortUpdMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling updateWorkEffort: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }
        Object assignPartyToWorkEffortShip_workEffortId = null;
        Object assignPartyToWorkEffortShip_partyId = null;
        Object assignPartyToWorkEffortShip_roleTypeId = null;
        Object assignPartyToWorkEffortShip_statusId = null;
        if ((!(UtilValidate.isEmpty(context.get("partyIdFrom"))) && !java.util.Objects.equals(context.get("partyIdFrom"), lookedUpValue.get("partyIdFrom")) && !(UtilValidate.isEmpty(lookedUpValue.get("estimatedShipWorkEffId"))))) {
            assignPartyToWorkEffortShip.put("workEffortId", lookedUpValue.get("estimatedShipWorkEffId"));
            assignPartyToWorkEffortShip.put("partyId", context.get("partyIdFrom"));
            try {
                existingShipWepas = EntityQuery.use(delegator)
                        .from("WorkEffortPartyAssignment")
                        .where(assignPartyToWorkEffortShip)
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying WorkEffortPartyAssignment: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            emptyField = EntityUtil.filterByDate(UtilGenerics.cast(existingShipWepas));
            if (UtilValidate.isEmpty(existingShipWepas)) {
                assignPartyToWorkEffortShip.put("roleTypeId", "CAL_ATTENDEE");
                assignPartyToWorkEffortShip.put("statusId", "CAL_SENT");
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("assignPartyToWorkEffort", assignPartyToWorkEffortShip);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling assignPartyToWorkEffort: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }
        Object assignPartyToWorkEffortArrival_workEffortId = null;
        Object assignPartyToWorkEffortArrival_partyId = null;
        Object assignPartyToWorkEffortArrival_roleTypeId = null;
        Object assignPartyToWorkEffortArrival_statusId = null;
        if ((!(UtilValidate.isEmpty(context.get("partyIdTo"))) && !java.util.Objects.equals(context.get("partyIdTo"), lookedUpValue.get("partyIdTo")) && !(UtilValidate.isEmpty(lookedUpValue.get("estimatedArrivalWorkEffId"))))) {
            assignPartyToWorkEffortArrival.put("workEffortId", lookedUpValue.get("estimatedArrivalWorkEffId"));
            assignPartyToWorkEffortArrival.put("partyId", context.get("partyIdTo"));
            try {
                existingArrivalWepas = EntityQuery.use(delegator)
                        .from("WorkEffortPartyAssignment")
                        .where(assignPartyToWorkEffortArrival)
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying WorkEffortPartyAssignment: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            emptyField = EntityUtil.filterByDate(UtilGenerics.cast(existingArrivalWepas));
            if (UtilValidate.isEmpty(existingArrivalWepas)) {
                assignPartyToWorkEffortArrival.put("roleTypeId", "CAL_ATTENDEE");
                assignPartyToWorkEffortArrival.put("statusId", "CAL_SENT");
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("assignPartyToWorkEffort", assignPartyToWorkEffortArrival);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling assignPartyToWorkEffort: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }
        result.put("oldStatusId", lookedUpValue.get("statusId"));
        result.put("oldPrimaryOrderId", lookedUpValue.get("primaryOrderId"));
        result.put("oldOriginFacilityId", lookedUpValue.get("originFacilityId"));
        result.put("oldDestinationFacilityId", lookedUpValue.get("destinationFacilityId"));
        lookedUpValue.setNonPKFields(context);
        Timestamp lookedUpValue_lastModifiedDate = new Timestamp(System.currentTimeMillis());
        lookedUpValue.put("lastModifiedByUserLogin", userLogin.get("userLoginId"));
        result.put("shipmentId", lookedUpValue.get("shipmentId"));
        try {
            delegator.store(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Delete Shipment
     */
    public static Map<String, Object> deleteShipment(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Object fromStatusId = null;
        Map<String, Object> inlineResult = null;
        Object operationName = "Delete Shipment";
        inlineResult = checkCanChangeShipmentStatusPacked(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("Shipment")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Shipment: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            delegator.removeValue(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Create Shipment based on ReturnHeader
     */
    public static Map<String, Object> createShipmentForReturn(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Map<String, Object> shipmentCtx = new HashMap<>();
        GenericValue returnHeader = null;
        try {
            returnHeader = EntityQuery.use(delegator)
                    .from("ReturnHeader")
                    .where(UtilMisc.toMap("returnId", context.get("returnId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ReturnHeader: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        shipmentCtx.put("partyIdFrom", returnHeader.get("fromPartyId"));
        shipmentCtx.put("partyIdTo", returnHeader.get("toPartyId"));
        shipmentCtx.put("originContactMechId", returnHeader.get("originContactMechId"));
        shipmentCtx.put("destinationFacilityId", returnHeader.get("destinationFacilityId"));
        shipmentCtx.put("primaryReturnId", returnHeader.get("returnId"));
        Object shipmentCtx_shipmentTypeId = null;
        Object shipmentCtx_statusId = null;
        if (returnHeader.get("returnHeaderTypeId") != null /* TODO: operator contains */) {
            shipmentCtx.put("shipmentTypeId", "SALES_RETURN");
            shipmentCtx.put("statusId", "PURCH_SHIP_CREATED");
        } else if ("VENDOR_RETURN".equals(returnHeader.get("returnHeaderTypeId"))) {
            shipmentCtx.put("shipmentTypeId", "PURCHASE_RETURN");
            shipmentCtx.put("statusId", "SHIPMENT_INPUT");
        } else {
            {
                String errorMsg = UtilProperties.getMessage("ProductUiLabels", "FacilityReturnHeaderTypeNotSupported", locale);
                error_list.add(errorMsg);
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        }
        Object shipmentId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createShipment", shipmentCtx);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            shipmentId = serviceResult.get("shipmentId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createShipment: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        result.put("shipmentId", shipmentId);

        return result;
    }


    /**
     * Create Shipment and ShipmentItems based on ReturnHeader and ReturnItems
     */
    public static Map<String, Object> createShipmentAndItemsForReturn(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue product = null;
        Boolean isPhysicalProductAvailable = null;
        GenericValue returnItem = null;
        Object isPhysicalProduct = null;
        Map<String, Object> shipmentCtx = new HashMap<>();
        Object shipmentId = null;
        Object shipItemCtx = null;
        Object shipmentItemSeqId = null;
        List<GenericValue> returnItems = null;
        try {
            returnItems = EntityQuery.use(delegator)
                    .from("ReturnItem")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ReturnItem: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        isPhysicalProductAvailable = Boolean.FALSE;
        if (returnItems != null) {
            for (GenericValue returnItem_iter : returnItems) {
                returnItem = returnItem_iter;
                try {
                    product = returnItem.getRelatedOne("Product", false);
                } catch (Exception e) {
                    Debug.logError(e, "Error getting related one Product: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (UtilValidate.isNotEmpty(product)) {
                    try {
                        isPhysicalProduct = ProductWorker.isPhysical((GenericValue) product);
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling ProductWorker.isPhysical: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    if (Boolean.TRUE.equals(isPhysicalProduct)) {
                        isPhysicalProductAvailable = Boolean.TRUE;
                    }
                }
            }
        }
        if (Boolean.TRUE.equals(isPhysicalProductAvailable)) {
            // set-service-fields from "parameters" to "shipmentCtx" for service "createShipmentForReturn"
            shipmentCtx.putAll(UtilMisc.toMap(context));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createShipmentForReturn", shipmentCtx);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
                shipmentId = serviceResult.get("shipmentId");
            } catch (Exception e) {
                Debug.logError(e, "Error calling createShipmentForReturn: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
            Debug.logInfo("Created new shipment " + shipmentId, MODULE);
            if (returnItems != null) {
                for (GenericValue returnItem_iter : returnItems) {
                    returnItem = returnItem_iter;
                    isPhysicalProduct = Boolean.FALSE;
                    try {
                        product = returnItem.getRelatedOne("Product", false);
                    } catch (Exception e) {
                        Debug.logError(e, "Error getting related one Product: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    if (UtilValidate.isNotEmpty(product)) {
                        try {
                            isPhysicalProduct = ProductWorker.isPhysical((GenericValue) product);
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling ProductWorker.isPhysical: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                    }
                    if (Boolean.TRUE.equals(isPhysicalProduct)) {
                        shipItemCtx = new HashMap<String, Object>();
                        ((Map<String, Object>) shipItemCtx).put("shipmentId", shipmentId);
                        ((Map<String, Object>) shipItemCtx).put("productId", returnItem.get("productId"));
                        ((Map<String, Object>) shipItemCtx).put("quantity", returnItem.get("returnQuantity"));
                        Debug.logInfo("calling create shipment item with " + shipItemCtx, MODULE);
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("createShipmentItem", (Map<String, Object>) shipItemCtx);
                            if (ServiceUtil.isError(serviceResult)) {
                                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                            }
                            shipmentItemSeqId = serviceResult.get("shipmentItemSeqId");
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling createShipmentItem: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                        shipItemCtx = new HashMap<String, Object>();
                        ((Map<String, Object>) shipItemCtx).put("shipmentId", shipmentId);
                        ((Map<String, Object>) shipItemCtx).put("shipmentItemSeqId", shipmentItemSeqId);
                        ((Map<String, Object>) shipItemCtx).put("returnId", returnItem.get("returnId"));
                        ((Map<String, Object>) shipItemCtx).put("returnItemSeqId", returnItem.get("returnItemSeqId"));
                        ((Map<String, Object>) shipItemCtx).put("quantity", returnItem.get("returnQuantity"));
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("createReturnItemShipment", (Map<String, Object>) shipItemCtx);
                            if (ServiceUtil.isError(serviceResult)) {
                                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                            }
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling createReturnItemShipment: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                    }
                }
            }
            result.put("shipmentId", shipmentId);
        }

        return result;
    }


    /**
     * Create Shipment and ShipmentItems based on primaryReturnId for Vendor return
     */
    public static Map<String, Object> createShipmentAndItemsForVendorReturn(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Object shipItemCtx = null;
        Object shipmentItemSeqId = null;
        Map<String, Object> shipmentCtx = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "shipmentCtx" for service "createShipment"
        shipmentCtx.putAll(UtilMisc.toMap(context));
        Object shipmentId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createShipment", shipmentCtx);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            shipmentId = serviceResult.get("shipmentId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createShipment: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        Debug.logInfo("Created new shipment " + shipmentId, MODULE);
        List<GenericValue> returnItems = null;
        try {
            returnItems = EntityQuery.use(delegator)
                    .from("ReturnItem")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ReturnItem: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (returnItems != null) {
            for (GenericValue returnItem : returnItems) {
                shipItemCtx = new HashMap<String, Object>();
                ((Map<String, Object>) shipItemCtx).put("shipmentId", shipmentId);
                ((Map<String, Object>) shipItemCtx).put("productId", returnItem.get("productId"));
                ((Map<String, Object>) shipItemCtx).put("quantity", returnItem.get("returnQuantity"));
                Debug.logInfo("calling create shipment item with " + shipItemCtx, MODULE);
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createShipmentItem", (Map<String, Object>) shipItemCtx);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                    shipmentItemSeqId = serviceResult.get("shipmentItemSeqId");
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createShipmentItem: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                shipItemCtx = new HashMap<String, Object>();
                ((Map<String, Object>) shipItemCtx).put("shipmentId", shipmentId);
                ((Map<String, Object>) shipItemCtx).put("shipmentItemSeqId", shipmentItemSeqId);
                ((Map<String, Object>) shipItemCtx).put("returnId", returnItem.get("returnId"));
                ((Map<String, Object>) shipItemCtx).put("returnItemSeqId", returnItem.get("returnItemSeqId"));
                ((Map<String, Object>) shipItemCtx).put("quantity", returnItem.get("returnQuantity"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createReturnItemShipment", (Map<String, Object>) shipItemCtx);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createReturnItemShipment: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }
        result.put("shipmentId", shipmentId);

        return result;
    }


    /**
     * Set Shipment Settings From Primary Order
     */
    public static Map<String, Object> setShipmentSettingsFromPrimaryOrder(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue orderItemShipGroup = null;
        GenericValue shipment = null;
        GenericValue productStore = null;
        Map<String, Object> limitRoleMap = null;
        List<GenericValue> limitOrderRoles = null;
        GenericValue limitOrderRole = null;
        Map<String, Object> destinationContactMap = null;
        List<GenericValue> destinationOrderContactMechs = null;
        GenericValue destinationOrderContactMech = null;
        List<GenericValue> originOrderContactMechs = null;
        GenericValue originOrderContactMech = null;
        Map<String, Object> originContactMap = null;
        GenericValue phoneNumber = null;
        Map<String, Object> destTelecomOrderContactMechMap = null;
        GenericValue destTelecomOrderContactMech = null;
        List<GenericValue> destTelecomOrderContactMechs = null;
        List<GenericValue> phoneNumbers = null;
        Map<String, Object> originTelecomOrderContactMechMap = null;
        GenericValue originTelecomOrderContactMech = null;
        List<GenericValue> originTelecomOrderContactMechs = null;
        Map<String, Object> facilityLookup = null;
        GenericValue destinationFacility = null;
        List<GenericValue> facilities = null;
        Map<String, Object> shipmentRouteSegmentMap = null;
        Object fromStatusId = null;
        Map<String, Object> inlineResult = null;
        Object operationName = "Set Shipment Settings From Primary Order";
        inlineResult = checkCanChangeShipmentStatusPacked(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        try {
            shipment = EntityQuery.use(delegator)
                    .from("Shipment")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Shipment: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(shipment.get("primaryOrderId"))) {
            Debug.logInfo("Not running setShipmentSettingsFromPrimaryOrder, primaryOrderId is empty for shipmentId [" + shipment.get("shipmentId") + "]", MODULE);
            return result;
        }
        if (UtilValidate.isEmpty(shipment.get("primaryShipGroupSeqId"))) {
            Debug.logInfo("Not running setShipmentSettingsFromPrimaryOrder, primaryShipGroupSeqId is empty for shipmentId [" + context.get("shipmentId") + "]", MODULE);
            return result;
        }
        GenericValue orderHeader = null;
        try {
            orderHeader = EntityQuery.use(delegator)
                    .from("OrderHeader")
                    .where(UtilMisc.toMap("orderId", shipment.get("primaryOrderId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderHeader: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(shipment.get("primaryShipGroupSeqId"))) {
            try {
                orderItemShipGroup = EntityQuery.use(delegator)
                        .from("OrderItemShipGroup")
                        .where(UtilMisc.toMap("orderId", shipment.get("primaryOrderId"), "shipGroupSeqId", shipment.get("primaryShipGroupSeqId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying OrderItemShipGroup: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        if ("SALES_ORDER".equals(orderHeader.get("orderTypeId"))) {
            shipment.put("shipmentTypeId", "SALES_SHIPMENT");
        }
        if ("PURCHASE_ORDER".equals(orderHeader.get("orderTypeId"))) {
            if (!"DROP_SHIPMENT".equals(shipment.get("shipmentTypeId"))) {
                shipment.put("shipmentTypeId", "PURCHASE_SHIPMENT");
            }
        }
        Object shipment_originFacilityId = null;
        if ((UtilValidate.isEmpty(shipment.get("originFacilityId")) && "SALES_SHIPMENT".equals(shipment.get("shipmentTypeId")) && !(UtilValidate.isEmpty(orderHeader.get("productStoreId"))))) {
            try {
                productStore = EntityQuery.use(delegator)
                        .from("ProductStore")
                        .where(UtilMisc.toMap("productStoreId", orderHeader.get("productStoreId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ProductStore: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if ("Y".equals(productStore.get("oneInventoryFacility"))) {
                shipment.put("originFacilityId", productStore.get("inventoryFacilityId"));
            }
        }
        List<GenericValue> orderRoles = null;
        try {
            orderRoles = EntityQuery.use(delegator)
                    .from("OrderRole")
                    .where(UtilMisc.toMap("orderId", shipment.get("primaryOrderId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(shipment.get("partyIdFrom"))) {
            limitRoleMap.put("roleTypeId", "SHIP_FROM_VENDOR");
            limitOrderRoles = EntityUtil.filterByAnd(orderRoles, limitRoleMap);
            limitOrderRole = EntityUtil.getFirst((List<GenericValue>) limitOrderRoles);
            if (UtilValidate.isNotEmpty(limitOrderRole)) {
                shipment.put("partyIdFrom", limitOrderRole.get("partyId"));
            }
            limitRoleMap = new HashMap<String, Object>();
            limitOrderRoles = null;
            limitOrderRole = null;
        }
        if (UtilValidate.isEmpty(shipment.get("partyIdFrom"))) {
            limitRoleMap.put("roleTypeId", "VENDOR");
            limitOrderRoles = EntityUtil.filterByAnd(orderRoles, limitRoleMap);
            limitOrderRole = EntityUtil.getFirst((List<GenericValue>) limitOrderRoles);
            if (UtilValidate.isNotEmpty(limitOrderRole)) {
                shipment.put("partyIdFrom", limitOrderRole.get("partyId"));
            }
            limitRoleMap = new HashMap<String, Object>();
            limitOrderRoles = null;
            limitOrderRole = null;
        }
        Debug.logInfo("setShipmentSettingsFromPrimaryOrder shipment.partyIdTo 1 ===> " + shipment.get("partyIdTo"), MODULE);
        if (UtilValidate.isEmpty(shipment.get("partyIdTo"))) {
            limitRoleMap.put("roleTypeId", "SHIP_TO_CUSTOMER");
            limitOrderRoles = EntityUtil.filterByAnd(orderRoles, limitRoleMap);
            limitOrderRole = EntityUtil.getFirst((List<GenericValue>) limitOrderRoles);
            if (UtilValidate.isNotEmpty(limitOrderRole)) {
                shipment.put("partyIdTo", limitOrderRole.get("partyId"));
            }
            limitRoleMap = new HashMap<String, Object>();
            limitOrderRoles = null;
            limitOrderRole = null;
        }
        if (UtilValidate.isEmpty(shipment.get("partyIdTo"))) {
            limitRoleMap.put("roleTypeId", "CUSTOMER");
            limitOrderRoles = EntityUtil.filterByAnd(orderRoles, limitRoleMap);
            limitOrderRole = EntityUtil.getFirst((List<GenericValue>) limitOrderRoles);
            if (UtilValidate.isNotEmpty(limitOrderRole)) {
                shipment.put("partyIdTo", limitOrderRole.get("partyId"));
            }
            limitRoleMap = new HashMap<String, Object>();
            limitOrderRoles = null;
            limitOrderRole = null;
        }
        Debug.logInfo("setShipmentSettingsFromPrimaryOrder shipment.partyIdTo 2 ===> " + shipment.get("partyIdTo"), MODULE);
        List<GenericValue> orderContactMechs = null;
        try {
            orderContactMechs = EntityQuery.use(delegator)
                    .from("OrderContactMech")
                    .where(UtilMisc.toMap("orderId", shipment.get("primaryOrderId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(shipment.get("destinationContactMechId"))) {
            destinationContactMap.put("contactMechPurposeTypeId", "SHIPPING_LOCATION");
            destinationOrderContactMechs = EntityUtil.filterByAnd(orderContactMechs, destinationContactMap);
            destinationOrderContactMech = EntityUtil.getFirst((List<GenericValue>) destinationOrderContactMechs);
            if (UtilValidate.isNotEmpty(destinationOrderContactMech)) {
                shipment.put("destinationContactMechId", destinationOrderContactMech.get("contactMechId"));
            } else {
                Debug.logWarning("Cannot find a shipping destination address for " + shipment.get("primaryOrderId"), MODULE);
            }
        }
        if (!"PURCHASE_SHIPMENT".equals(shipment.get("shipmentTypeId"))) {
            if (UtilValidate.isEmpty(shipment.get("originContactMechId"))) {
                originContactMap.put("contactMechPurposeTypeId", "SHIP_ORIG_LOCATION");
                originOrderContactMechs = EntityUtil.filterByAnd(orderContactMechs, originContactMap);
                originOrderContactMech = EntityUtil.getFirst((List<GenericValue>) originOrderContactMechs);
                if (UtilValidate.isNotEmpty(originOrderContactMech)) {
                    shipment.put("originContactMechId", originOrderContactMech.get("contactMechId"));
                } else {
                    Debug.logWarning("Cannot find a shipping origin address for " + shipment.get("primaryOrderId"), MODULE);
                }
            }
        }
        if (UtilValidate.isEmpty(shipment.get("destinationTelecomNumberId"))) {
            destTelecomOrderContactMechMap.put("contactMechPurposeTypeId", "PHONE_SHIPPING");
            destTelecomOrderContactMechs = EntityUtil.filterByAnd(orderContactMechs, destTelecomOrderContactMechMap);
            destTelecomOrderContactMech = EntityUtil.getFirst((List<GenericValue>) destTelecomOrderContactMechs);
            if (UtilValidate.isNotEmpty(destTelecomOrderContactMech)) {
                shipment.put("destinationTelecomNumberId", destTelecomOrderContactMech.get("contactMechId"));
            } else {
                try {
                    phoneNumbers = EntityQuery.use(delegator)
                            .from("PartyAndTelecomNumber")
                            .where(UtilMisc.toMap("partyId", shipment.get("partyIdTo")))
                            .filterByDate()
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                phoneNumber = EntityUtil.getFirst((List<GenericValue>) phoneNumbers);
                if (UtilValidate.isNotEmpty(phoneNumber)) {
                    shipment.put("destinationTelecomNumberId", phoneNumber.get("contactMechId"));
                } else {
                    Debug.logWarning("Cannot find a shipping destination phone number for " + shipment.get("primaryOrderId"), MODULE);
                }
            }
        }
        if (UtilValidate.isEmpty(shipment.get("originTelecomNumberId"))) {
            originTelecomOrderContactMechMap.put("contactMechPurposeTypeId", "PHONE_SHIP_ORIG");
            originTelecomOrderContactMechs = EntityUtil.filterByAnd(orderContactMechs, originTelecomOrderContactMechMap);
            originTelecomOrderContactMech = EntityUtil.getFirst((List<GenericValue>) originTelecomOrderContactMechs);
            if (UtilValidate.isNotEmpty(originTelecomOrderContactMech)) {
                shipment.put("originTelecomNumberId", originTelecomOrderContactMech.get("contactMechId"));
            } else {
                Debug.logWarning("Cannot find a shipping origin phone number for " + shipment.get("primaryOrderId"), MODULE);
            }
        }
        if (UtilValidate.isEmpty(shipment.get("destinationFacilityId"))) {
            if ("PURCHASE_SHIPMENT".equals(shipment.get("shipmentTypeId"))) {
                facilityLookup.put("contactMechId", shipment.get("destinationContactMechId"));
                try {
                    facilities = EntityQuery.use(delegator)
                            .from("FacilityContactMech")
                            .where(facilityLookup)
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying FacilityContactMech: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                destinationFacility = EntityUtil.getFirst((List<GenericValue>) facilities);
                shipment.put("destinationFacilityId", destinationFacility.get("facilityId"));
            }
        }
        if (UtilValidate.isNotEmpty(orderItemShipGroup)) {
            if ("SALES_ORDER".equals(orderHeader.get("orderTypeId"))) {
                shipment.put("destinationContactMechId", orderItemShipGroup.get("contactMechId"));
                shipment.put("destinationTelecomNumberId", orderItemShipGroup.get("telecomContactMechId"));
            }
        }
        if (UtilValidate.isEmpty(shipment.get("estimatedShipCost"))) {
            try {
                Map<String, Object> scriptContext = new HashMap<String, Object>();
                scriptContext.put("delegator", delegator);
                scriptContext.put("dispatcher", dispatcher);
                scriptContext.put("locale", locale);
                scriptContext.put("userLogin", userLogin);
                scriptContext.put("context", context);
                scriptContext.put("parameters", context);
                Object scriptResult = GroovyUtil.eval("import java.math.BigDecimal\n                    import org.ofbiz.order.order.OrderReadHelper\n\n                    orderReadHelper = new OrderReadHelper(orderHeader)\n                    orderItems = orderReadHelper.getValidOrderItems()\n                    orderAdjustments = orderReadHelper.getAdjustments()\n                    orderHeaderAdjustments = orderReadHelper.getOrderHeaderAdjustments()\n                    orderSubTotal = orderReadHelper.getOrderItemsSubTotal()\n\n                    shippingAmount = OrderReadHelper.getAllOrderItemsAdjustmentsTotal(orderItems, orderAdjustments, false, false, true)\n                    shippingAmount = shippingAmount.add(OrderReadHelper.calcOrderAdjustments(orderHeaderAdjustments, orderSubTotal, false, false, true))\n                    //org.ofbiz.base.util.Debug.log(\"shippingAmmount=\" + shippingAmount)\n                    shipment.put(\"estimatedShipCost\", shippingAmount)", scriptContext);
            } catch (Exception e) {
                Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
            }
        }
        Map<String, Object> shipmentUpdateMap = new HashMap<String, Object>();
        // set-service-fields from "shipment" to "shipmentUpdateMap" for service "updateShipment"
        shipmentUpdateMap.putAll(UtilMisc.toMap(shipment));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateShipment", shipmentUpdateMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateShipment: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        shipmentRouteSegmentMap.put("shipmentId", shipment.get("shipmentId"));
        List<GenericValue> shipmentRouteSegments = null;
        try {
            shipmentRouteSegments = EntityQuery.use(delegator)
                    .from("ShipmentRouteSegment")
                    .where(shipmentRouteSegmentMap)
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ShipmentRouteSegment: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(shipmentRouteSegments)) {
            shipmentRouteSegmentMap.put("estimatedStartDate", shipment.get("estimatedShipDate"));
            shipmentRouteSegmentMap.put("estimatedArrivalDate", shipment.get("estimatedArrivalDate"));
            shipmentRouteSegmentMap.put("originFacilityId", shipment.get("originFacilityId"));
            shipmentRouteSegmentMap.put("originContactMechId", shipment.get("originContactMechId"));
            shipmentRouteSegmentMap.put("originTelecomNumberId", shipment.get("originTelecomNumberId"));
            shipmentRouteSegmentMap.put("destFacilityId", shipment.get("destinationFacilityId"));
            shipmentRouteSegmentMap.put("destContactMechId", shipment.get("destinationContactMechId"));
            shipmentRouteSegmentMap.put("destTelecomNumberId", shipment.get("destinationTelecomNumberId"));
            shipmentRouteSegmentMap.put("statusId", null);
            try {
                orderItemShipGroup = EntityQuery.use(delegator)
                        .from("OrderItemShipGroup")
                        .where(UtilMisc.toMap("orderId", shipment.get("primaryOrderId"), "shipGroupSeqId", shipment.get("primaryShipGroupSeqId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying OrderItemShipGroup: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (UtilValidate.isNotEmpty(orderItemShipGroup)) {
                shipmentRouteSegmentMap.put("carrierPartyId", orderItemShipGroup.get("carrierPartyId"));
                shipmentRouteSegmentMap.put("shipmentMethodTypeId", orderItemShipGroup.get("shipmentMethodTypeId"));
            }
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createShipmentRouteSegment", shipmentRouteSegmentMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling createShipmentRouteSegment: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        return result;
    }


    /**
     * Set Shipment Settings From Facilities
     */
    public static Map<String, Object> setShipmentSettingsFromFacilities(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Map<String, Object> shipmentUpdateMap = null;
        Object fromStatusId = null;
        Map<String, Object> inlineResult = null;
        Object operationName = "Set Shipment Settings From Facilities";
        inlineResult = checkCanChangeShipmentStatusPacked(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue shipment = null;
        try {
            shipment = EntityQuery.use(delegator)
                    .from("Shipment")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Shipment: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue shipmentCopy = GenericValue.create((GenericValue) shipment);
        List<String> descendingFromDateOrder = new ArrayList<>();
        descendingFromDateOrder.add("-fromDate");
        if (UtilValidate.isNotEmpty(shipment.get("originFacilityId"))) {
            if (UtilValidate.isEmpty(shipment.get("originContactMechId"))) {
                try {
                    Map<String, Object> scriptContext = new HashMap<String, Object>();
                    scriptContext.put("delegator", delegator);
                    scriptContext.put("dispatcher", dispatcher);
                    scriptContext.put("locale", locale);
                    scriptContext.put("userLogin", userLogin);
                    scriptContext.put("context", context);
                    scriptContext.put("parameters", context);
                    Object scriptResult = GroovyUtil.eval("facilityContactMech = org.ofbiz.party.contact.ContactMechWorker.getFacilityContactMechByPurpose(\n                                delegator, shipment.get(\"originFacilityId\"), \n                                org.ofbiz.base.util.UtilMisc.toList(\"SHIP_ORIG_LOCATION\", \"PRIMARY_LOCATION\"))\n                    if (facilityContactMech != null) {\n                        shipment.put(\"originContactMechId\", facilityContactMech.get(\"contactMechId\"));\n                    }", scriptContext);
                } catch (Exception e) {
                    Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
                }
            }
            if (UtilValidate.isEmpty(shipment.get("originTelecomNumberId"))) {
                try {
                    Map<String, Object> scriptContext = new HashMap<String, Object>();
                    scriptContext.put("delegator", delegator);
                    scriptContext.put("dispatcher", dispatcher);
                    scriptContext.put("locale", locale);
                    scriptContext.put("userLogin", userLogin);
                    scriptContext.put("context", context);
                    scriptContext.put("parameters", context);
                    Object scriptResult = GroovyUtil.eval("facilityContactMech = org.ofbiz.party.contact.ContactMechWorker.getFacilityContactMechByPurpose(\n                                delegator, shipment.get(\"originFacilityId\"), \n                                org.ofbiz.base.util.UtilMisc.toList(\"PHONE_SHIP_ORIG\", \"PRIMARY_PHONE\"))\n                    if (facilityContactMech != null) {\n                        shipment.put(\"originTelecomNumberId\", facilityContactMech.get(\"contactMechId\"))\n                    }", scriptContext);
                } catch (Exception e) {
                    Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
                }
            }
        }
        if (UtilValidate.isNotEmpty(shipment.get("destinationFacilityId"))) {
            if (UtilValidate.isEmpty(shipment.get("destinationContactMechId"))) {
                try {
                    Map<String, Object> scriptContext = new HashMap<String, Object>();
                    scriptContext.put("delegator", delegator);
                    scriptContext.put("dispatcher", dispatcher);
                    scriptContext.put("locale", locale);
                    scriptContext.put("userLogin", userLogin);
                    scriptContext.put("context", context);
                    scriptContext.put("parameters", context);
                    Object scriptResult = GroovyUtil.eval("facilityContactMech = org.ofbiz.party.contact.ContactMechWorker.getFacilityContactMechByPurpose(\n                                delegator, shipment.get(\"destinationFacilityId\"), \n                                org.ofbiz.base.util.UtilMisc.toList(\"SHIPPING_LOCATION\", \"PRIMARY_LOCATION\"))\n                    if (facilityContactMech != null) {\n                        shipment.put(\"destinationContactMechId\", facilityContactMech.get(\"contactMechId\"))\n                    }", scriptContext);
                } catch (Exception e) {
                    Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
                }
            }
            if (UtilValidate.isEmpty(shipment.get("destinationTelecomNumberId"))) {
                try {
                    Map<String, Object> scriptContext = new HashMap<String, Object>();
                    scriptContext.put("delegator", delegator);
                    scriptContext.put("dispatcher", dispatcher);
                    scriptContext.put("locale", locale);
                    scriptContext.put("userLogin", userLogin);
                    scriptContext.put("context", context);
                    scriptContext.put("parameters", context);
                    Object scriptResult = GroovyUtil.eval("facilityContactMech = org.ofbiz.party.contact.ContactMechWorker.getFacilityContactMechByPurpose(\n                                delegator, shipment.get(\"destinationFacilityId\"), \n                                org.ofbiz.base.util.UtilMisc.toList(\"PHONE_SHIPPING\", \"PRIMARY_PHONE\"))\n                    if (facilityContactMech != null) {\n                        shipment.put(\"destinationTelecomNumberId\", facilityContactMech.get(\"contactMechId\"))\n                    }", scriptContext);
                } catch (Exception e) {
                    Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
                }
            }
        }
        if (!java.util.Objects.equals(shipment, shipmentCopy)) {
            // set-service-fields from "shipment" to "shipmentUpdateMap" for service "updateShipment"
            shipmentUpdateMap.putAll(UtilMisc.toMap(shipment));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updateShipment", shipmentUpdateMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling updateShipment: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        return result;
    }


    /**
     * Send Shipment Scheduled Notification
     */
    public static Map<String, Object> sendShipmentScheduledNotification(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> sendToPartyIdMap = null;
        List<GenericValue> sendToPartyPartyAndContactMechs = null;
        GenericValue shipment = null;
        try {
            shipment = EntityQuery.use(delegator)
                    .from("Shipment")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Shipment: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        List<GenericValue> curUserPartyAndContactMechs = null;
        try {
            curUserPartyAndContactMechs = EntityQuery.use(delegator)
                    .from("PartyAndContactMech")
                    .where(UtilMisc.toMap("partyId", userLogin.get("partyId"), "contactMechTypeId", "EMAIL_ADDRESS"))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue curUserPartyAndContactMech = EntityUtil.getFirst((List<GenericValue>) curUserPartyAndContactMechs);
        String sendEmailMap_sendFrom = "," + "";
        sendToPartyIdMap.put((String) shipment.get("partyIdFrom"), shipment.get("partyIdFrom"));
        List<GenericValue> supplierAgentOrderRoles = null;
        try {
            supplierAgentOrderRoles = EntityQuery.use(delegator)
                    .from("OrderRole")
                    .where(UtilMisc.toMap("orderId", shipment.get("primaryOrderId"), "roleTypeId", "SUPPLIER_AGENT"))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (supplierAgentOrderRoles != null) {
            for (GenericValue supplierAgentOrderRole : supplierAgentOrderRoles) {
                sendToPartyIdMap.put((String) supplierAgentOrderRole.get("partyId"), supplierAgentOrderRole.get("partyId"));
            }
        }
        for (Map.Entry<String, Object> entry : ((Map<String, Object>) sendToPartyIdMap).entrySet()) {
            String sendToPartyId = entry.getKey();
            Object sendToPartyIdValue = entry.getValue();
            try {
                sendToPartyPartyAndContactMechs = EntityQuery.use(delegator)
                        .from("PartyAndContactMech")
                        .where(UtilMisc.toMap("partyId", sendToPartyId, "contactMechTypeId", "EMAIL_ADDRESS"))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (sendToPartyPartyAndContactMechs != null) {
                for (GenericValue sendToPartyPartyAndContactMech : sendToPartyPartyAndContactMechs) {
                    String sendEmailMap_sendTo = "," + "";
                }
            }
        }
        Map<String, Object> sendEmailMap = new HashMap<String, Object>();
        sendEmailMap.put("subject", "Scheduled Notification for Shipment " + shipment.get("shipmentId"));
        if (UtilValidate.isNotEmpty(shipment.get("primaryOrderId"))) {
            String sendEmailMap_subject = "";
        }
        sendEmailMap.put("contentType", "text/html");
        sendEmailMap.put("templateName", "org/ofbiz/shipment/shipment/ShipmentScheduledNotice.ftl");
        sendEmailMap.put("templateData.shipment", shipment);
        Debug.logInfo("Sending generic notification email (if all info is in place): " + sendEmailMap, MODULE);
        if ((!(UtilValidate.isEmpty(((Map<String, Object>) sendEmailMap).get("sendTo"))) && !(UtilValidate.isEmpty(((Map<String, Object>) sendEmailMap).get("sendFrom"))))) {
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("sendGenericNotificationEmail", sendEmailMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling sendGenericNotificationEmail: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        } else {
            Debug.logError("Insufficient data to send notice email: " + sendEmailMap, MODULE);
        }

        return result;
    }


    /**
     * Release the purchase order's items assigned to the shipment but not actually received
     */
    public static Map<String, Object> balanceItemIssuancesForShipment(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<GenericValue> receipts = null;
        Object issuanceQuantity = null;
        GenericValue shipment = null;
        try {
            shipment = EntityQuery.use(delegator)
                    .from("Shipment")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Shipment: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        List<GenericValue> issuances = null;
        try {
            issuances = shipment.getRelated("ItemIssuance", null, null, false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related ItemIssuance: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (issuances != null) {
            for (GenericValue issuance : issuances) {
                try {
                    receipts = EntityQuery.use(delegator)
                            .from("ShipmentReceipt")
                            .where(UtilMisc.toMap("shipmentId", shipment.get("shipmentId"), "orderId", issuance.get("orderId"), "orderItemSeqId", issuance.get("orderItemSeqId")))
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (receipts != null) {
                    for (GenericValue receipt : receipts) {
                        issuanceQuantity = new BigDecimal(receipt.get("quantityAccepted").toString());
                    }
                }
                issuance.put("quantity", issuanceQuantity);
                try {
                    delegator.store(issuance);
                } catch (Exception e) {
                    Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                issuanceQuantity = null;
            }
        }

        return result;
    }


    /**
     * Create ShipmentItem
     */
    public static Map<String, Object> createShipmentItem(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Object fromStatusId = null;
        Map<String, Object> inlineResult = null;
        Object operationName = "Create ShipmentItem";
        inlineResult = checkCanChangeShipmentStatusPacked(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue newEntity = delegator.makeValue("ShipmentItem");
        newEntity.setPKFields(context);
        newEntity.setNonPKFields(context);
        delegator.setNextSubSeqId(newEntity, "shipmentItemSeqId", 5, 1);
        Object shipmentItemSeqId = newEntity.get("shipmentItemSeqId");
        result.put("shipmentItemSeqId", newEntity.get("shipmentItemSeqId"));
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Update ShipmentItem
     */
    public static Map<String, Object> updateShipmentItem(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Object fromStatusId = null;
        Map<String, Object> inlineResult = null;
        Object operationName = "Update ShipmentItem";
        inlineResult = checkCanChangeShipmentStatusPacked(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue lookupPKMap = delegator.makeValue("ShipmentItem");
        lookupPKMap.setPKFields(context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ShipmentItem")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ShipmentItem: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        lookedUpValue.setNonPKFields(context);
        try {
            delegator.store(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Delete ShipmentItem
     */
    public static Map<String, Object> deleteShipmentItem(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue lookedUpValue = null;
        GenericValue lookupPKMap = null;
        Object fromStatusId = null;
        Map<String, Object> inlineResult = null;
        Object operationName = "Delete ShipmentItem";
        inlineResult = checkCanChangeShipmentStatusPacked(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        List<GenericValue> shipmentPackageContents = null;
        try {
            shipmentPackageContents = EntityQuery.use(delegator)
                    .from("ShipmentPackageContent")
                    .where(UtilMisc.toMap("shipmentId", context.get("shipmentId"), "shipmentItemSeqId", context.get("shipmentItemSeqId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(shipmentPackageContents)) {
            {
                String errorMsg = UtilProperties.getMessage("ProductErrorUiLabels", "ProductErrorShipmentItemCannotBeDeleted", locale);
                error_list.add(errorMsg);
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        } else {
            lookupPKMap = delegator.makeValue("ShipmentItem");
            lookupPKMap.setPKFields(context);
            try {
                lookedUpValue = EntityQuery.use(delegator)
                        .from("ShipmentItem")
                        .where(lookupPKMap)
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error finding by primary key ShipmentItem: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            try {
                delegator.removeValue(lookedUpValue);
            } catch (Exception e) {
                Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        return result;
    }


    /**
     * splitShipmentItemByQuantity
     */
    public static Map<String, Object> splitShipmentItemByQuantity(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> updateOrderShipmentMap = null;
        Object orderShipmentQuantityLeft = null;
        Object createOrderShipmentMap = null;
        Map<String, Object> deleteOrderShipmentMap = null;
        GenericValue originalShipmentItem = null;
        try {
            originalShipmentItem = EntityQuery.use(delegator)
                    .from("ShipmentItem")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ShipmentItem: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Object inputMap = null;
        ((Map<String, Object>) inputMap).put("shipmentId", originalShipmentItem.get("shipmentId"));
        ((Map<String, Object>) inputMap).put("productId", originalShipmentItem.get("productId"));
        ((Map<String, Object>) inputMap).put("quantity", context.get("newItemQuantity"));
        Object newShipmentItemSeqId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createShipmentItem", (Map<String, Object>) inputMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            newShipmentItemSeqId = serviceResult.get("shipmentItemSeqId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createShipmentItem: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        originalShipmentItem.set("quantity", new BigDecimal(context.get("newItemQuantity").toString()));
        Map<String, Object> updateOriginalShipmentItemMap = new HashMap<String, Object>();
        // set-service-fields from "originalShipmentItem" to "updateOriginalShipmentItemMap" for service "updateShipmentItem"
        updateOriginalShipmentItemMap.putAll(UtilMisc.toMap(originalShipmentItem));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateShipmentItem", updateOriginalShipmentItemMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateShipmentItem: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        List<GenericValue> itemOrderShipmentList = null;
        try {
            itemOrderShipmentList = EntityQuery.use(delegator)
                    .from("OrderShipment")
                    .where(UtilMisc.toMap("shipmentId", originalShipmentItem.get("shipmentId"), "shipmentItemSeqId", originalShipmentItem.get("shipmentItemSeqId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        orderShipmentQuantityLeft = context.get("newItemQuantity");
        if (itemOrderShipmentList != null) {
            for (GenericValue itemOrderShipment : itemOrderShipmentList) {
                if (((Comparable) orderShipmentQuantityLeft).compareTo(BigDecimal.ZERO) > 0) {
                    if (itemOrderShipment.get("quantity") != null /* TODO: field compare operator greater */) {
                        updateOrderShipmentMap = new HashMap<String, Object>();
                        // set-service-fields from "itemOrderShipment" to "updateOrderShipmentMap" for service "updateOrderShipment"
                        updateOrderShipmentMap.putAll(UtilMisc.toMap(itemOrderShipment));
                        ((Map<String, Object>) updateOrderShipmentMap).put("quantity", new BigDecimal(orderShipmentQuantityLeft.toString()));
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("updateOrderShipment", updateOrderShipmentMap);
                            if (ServiceUtil.isError(serviceResult)) {
                                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                            }
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling updateOrderShipment: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                        createOrderShipmentMap = new HashMap<String, Object>();
                        ((Map<String, Object>) createOrderShipmentMap).put("orderId", itemOrderShipment.get("orderId"));
                        ((Map<String, Object>) createOrderShipmentMap).put("orderItemSeqId", itemOrderShipment.get("orderItemSeqId"));
                        ((Map<String, Object>) createOrderShipmentMap).put("shipmentId", itemOrderShipment.get("shipmentId"));
                        ((Map<String, Object>) createOrderShipmentMap).put("shipmentItemSeqId", newShipmentItemSeqId);
                        ((Map<String, Object>) createOrderShipmentMap).put("quantity", orderShipmentQuantityLeft);
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("createOrderShipment", (Map<String, Object>) createOrderShipmentMap);
                            if (ServiceUtil.isError(serviceResult)) {
                                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                            }
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling createOrderShipment: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                        orderShipmentQuantityLeft = BigDecimal.ZERO;
                    } else {
                        deleteOrderShipmentMap = new HashMap<String, Object>();
                        // set-service-fields from "itemOrderShipment" to "deleteOrderShipmentMap" for service "deleteOrderShipment"
                        deleteOrderShipmentMap.putAll(UtilMisc.toMap(itemOrderShipment));
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("deleteOrderShipment", deleteOrderShipmentMap);
                            if (ServiceUtil.isError(serviceResult)) {
                                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                            }
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling deleteOrderShipment: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                        createOrderShipmentMap = new HashMap<String, Object>();
                        ((Map<String, Object>) createOrderShipmentMap).put("orderId", itemOrderShipment.get("orderId"));
                        ((Map<String, Object>) createOrderShipmentMap).put("orderItemSeqId", itemOrderShipment.get("orderItemSeqId"));
                        ((Map<String, Object>) createOrderShipmentMap).put("shipmentId", itemOrderShipment.get("shipmentId"));
                        ((Map<String, Object>) createOrderShipmentMap).put("shipmentItemSeqId", newShipmentItemSeqId);
                        ((Map<String, Object>) createOrderShipmentMap).put("quantity", itemOrderShipment.get("quantity"));
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("createOrderShipment", (Map<String, Object>) createOrderShipmentMap);
                            if (ServiceUtil.isError(serviceResult)) {
                                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                            }
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling createOrderShipment: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                        orderShipmentQuantityLeft = new BigDecimal(orderShipmentQuantityLeft.toString());
                    }
                }
            }
        }
        result.put("newShipmentItemSeqId", newShipmentItemSeqId);

        return result;
    }


    /**
     * Create ShipmentPackage
     */
    public static Map<String, Object> createShipmentPackage(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Object fromStatusId = null;
        Map<String, Object> inlineResult = null;
        GenericValue checkShipmentPackageRouteSeg = null;
        GenericValue shipmentRouteSegment = null;
        Map<String, Object> checkShipmentPackageRouteSegMap = null;
        List<GenericValue> shipmentRouteSegments = null;
        Object operationName = "Create ShipmentPackage";
        inlineResult = checkCanChangeShipmentStatusPacked(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue newEntity = delegator.makeValue("ShipmentPackage");
        newEntity.setPKFields(context);
        newEntity.setNonPKFields(context);
        if ("New".equals(newEntity.get("shipmentPackageSeqId"))) {
            newEntity.remove("shipmentPackageSeqId");
        }
        delegator.setNextSubSeqId(newEntity, "shipmentPackageSeqId", 5, 1);
        Object shipmentPackageSeqId = newEntity.get("shipmentPackageSeqId");
        result.put("shipmentPackageSeqId", newEntity.get("shipmentPackageSeqId"));
        Timestamp newEntity_dateCreated = new Timestamp(System.currentTimeMillis());
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Object shipmentId = newEntity.get("shipmentId");
        shipmentPackageSeqId = newEntity.get("shipmentPackageSeqId");
        inlineResult = ensurePackageRouteSeg(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }

        return result;
    }


    /**
     * Update ShipmentPackage
     */
    public static Map<String, Object> updateShipmentPackage(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Object fromStatusId = null;
        Map<String, Object> inlineResult = null;
        GenericValue checkShipmentPackageRouteSeg = null;
        GenericValue shipmentRouteSegment = null;
        Map<String, Object> checkShipmentPackageRouteSegMap = null;
        List<GenericValue> shipmentRouteSegments = null;
        Object operationName = "Update ShipmentPackage";
        inlineResult = checkCanChangeShipmentStatusShipped(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue lookupPKMap = delegator.makeValue("ShipmentPackage");
        lookupPKMap.setPKFields(context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ShipmentPackage")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ShipmentPackage: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        lookedUpValue.setNonPKFields(context);
        try {
            delegator.store(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Object shipmentId = lookedUpValue.get("shipmentId");
        Object shipmentPackageSeqId = lookedUpValue.get("shipmentPackageSeqId");
        inlineResult = ensurePackageRouteSeg(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }

        return result;
    }


    /**
     * Delete ShipmentPackage
     */
    public static Map<String, Object> deleteShipmentPackage(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue lookedUpValue = null;
        Object fromStatusId = null;
        Map<String, Object> inlineResult = null;
        Object operationName = "Delete ShipmentPackage";
        inlineResult = checkCanChangeShipmentStatusPacked(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        List<GenericValue> shipmentPackageContents = null;
        try {
            shipmentPackageContents = EntityQuery.use(delegator)
                    .from("ShipmentPackageContent")
                    .where(UtilMisc.toMap("shipmentId", context.get("shipmentId"), "shipmentPackageSeqId", context.get("shipmentPackageSeqId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(shipmentPackageContents)) {
            {
                String errorMsg = UtilProperties.getMessage("ProductErrorUiLabels", "ProductErrorShipmentPackageCannotBeDeleted", locale);
                error_list.add(errorMsg);
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        } else {
            try {
                lookedUpValue = EntityQuery.use(delegator)
                        .from("ShipmentPackage")
                        .where(context)
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ShipmentPackage: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            try {
                delegator.removeValue(lookedUpValue);
            } catch (Exception e) {
                Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        return result;
    }


    /**
     * Ensure ShipmentPackageRouteSeg exists for all RouteSegments for this Package
     */
    public static Map<String, Object> ensurePackageRouteSeg(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue checkShipmentPackageRouteSeg = null;
        Map<String, Object> checkShipmentPackageRouteSegMap = null;
        List<GenericValue> shipmentRouteSegments = null;
        try {
            shipmentRouteSegments = EntityQuery.use(delegator)
                    .from("ShipmentRouteSegment")
                    .where(UtilMisc.toMap("shipmentId", context.get("shipmentId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (shipmentRouteSegments != null) {
            for (GenericValue shipmentRouteSegment : shipmentRouteSegments) {
                try {
                    checkShipmentPackageRouteSeg = EntityQuery.use(delegator)
                            .from("ShipmentPackageRouteSeg")
                            .where(UtilMisc.toMap("shipmentId", context.get("shipmentId"), "shipmentPackageSeqId", context.get("shipmentPackageSeqId"), "shipmentRouteSegmentId", shipmentRouteSegment.get("shipmentRouteSegmentId")))
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying ShipmentPackageRouteSeg: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (UtilValidate.isEmpty(checkShipmentPackageRouteSeg)) {
                    checkShipmentPackageRouteSegMap.put("shipmentRouteSegmentId", shipmentRouteSegment.get("shipmentRouteSegmentId"));
                    checkShipmentPackageRouteSegMap.put("shipmentPackageSeqId", context.get("shipmentPackageSeqId"));
                    checkShipmentPackageRouteSegMap.put("shipmentId", context.get("shipmentId"));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("createShipmentPackageRouteSeg", checkShipmentPackageRouteSegMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling createShipmentPackageRouteSeg: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            }
        }

        return result;
    }


    /**
     * Create ShipmentPackageContent
     */
    public static Map<String, Object> createShipmentPackageContent(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Object fromStatusId = null;
        Map<String, Object> inlineResult = null;
        Object operationName = "Create ShipmentPackageContent";
        inlineResult = checkCanChangeShipmentStatusPacked(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue newEntity = delegator.makeValue("ShipmentPackageContent");
        newEntity.setPKFields(context);
        newEntity.setNonPKFields(context);
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        result.put("shipmentPackageSeqId", newEntity.get("shipmentPackageSeqId"));

        return result;
    }


    /**
     * Update ShipmentPackageContent
     */
    public static Map<String, Object> updateShipmentPackageContent(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Object fromStatusId = null;
        Map<String, Object> inlineResult = null;
        Object operationName = "Update ShipmentPackageContent";
        inlineResult = checkCanChangeShipmentStatusPacked(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue lookupPKMap = delegator.makeValue("ShipmentPackageContent");
        lookupPKMap.setPKFields(context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ShipmentPackageContent")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ShipmentPackageContent: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        lookedUpValue.setNonPKFields(context);
        try {
            delegator.store(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Delete ShipmentPackageContent
     */
    public static Map<String, Object> deleteShipmentPackageContent(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Object fromStatusId = null;
        Map<String, Object> inlineResult = null;
        Object operationName = "Delete ShipmentPackageContent";
        inlineResult = checkCanChangeShipmentStatusPacked(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue lookupPKMap = delegator.makeValue("ShipmentPackageContent");
        lookupPKMap.setPKFields(context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ShipmentPackageContent")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ShipmentPackageContent: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            delegator.removeValue(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Add Shipment Content To Package
     */
    public static Map<String, Object> addShipmentContentToPackage(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue newEntity = null;
        Map<String, Object> createSPCMap = null;
        Map<String, Object> updateSPCMap = null;
        Object fromStatusId = null;
        Map<String, Object> inlineResult = null;
        Object operationName = "Create ShipmentPackageContent";
        inlineResult = checkCanChangeShipmentStatusPacked(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        newEntity = delegator.makeValue("ShipmentPackageContent");
        newEntity.setPKFields(context);
        GenericValue shipmentPackageContent = null;
        try {
            shipmentPackageContent = EntityQuery.use(delegator)
                    .from(newEntity.getEntityName())
                    .where(newEntity)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key : " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Debug.logVerbose("In addShipmentContentToPackage trying values: " + newEntity, MODULE);
        if (UtilValidate.isEmpty(shipmentPackageContent)) {
            // set-service-fields from "parameters" to "createSPCMap" for service "createShipmentPackageContent"
            createSPCMap.putAll(UtilMisc.toMap(context));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createShipmentPackageContent", createSPCMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
                newEntity.put("shipmentPackageSeqId", serviceResult.get("shipmentPackageSeqId"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling createShipmentPackageContent: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        } else {
            shipmentPackageContent.set("quantity", new BigDecimal(shipmentPackageContent.get("quantity").toString()));
            // set-service-fields from "shipmentPackageContent" to "updateSPCMap" for service "updateShipmentPackageContent"
            updateSPCMap.putAll(UtilMisc.toMap(shipmentPackageContent));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updateShipmentPackageContent", updateSPCMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling updateShipmentPackageContent: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        Debug.logInfo("Shipment package: " + newEntity, MODULE);
        result.put("shipmentPackageSeqId", newEntity.get("shipmentPackageSeqId"));

        return result;
    }


    /**
     * Create ShipmentPackageRouteSeg
     */
    public static Map<String, Object> createShipmentPackageRouteSeg(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue newEntity = delegator.makeValue("ShipmentPackageRouteSeg");
        newEntity.setPKFields(context);
        newEntity.setNonPKFields(context);
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Update ShipmentPackageRouteSeg
     */
    public static Map<String, Object> updateShipmentPackageRouteSeg(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookupPKMap = delegator.makeValue("ShipmentPackageRouteSeg");
        lookupPKMap.setPKFields(context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ShipmentPackageRouteSeg")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ShipmentPackageRouteSeg: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        lookedUpValue.setNonPKFields(context);
        try {
            delegator.store(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Delete ShipmentPackageRouteSeg
     */
    public static Map<String, Object> deleteShipmentPackageRouteSeg(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Object fromStatusId = null;
        Map<String, Object> inlineResult = null;
        Object operationName = "Delete ShipmentPackageRouteSeg";
        inlineResult = checkCanChangeShipmentStatusPacked(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue lookupPKMap = delegator.makeValue("ShipmentPackageRouteSeg");
        lookupPKMap.setPKFields(context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ShipmentPackageRouteSeg")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ShipmentPackageRouteSeg: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            delegator.removeValue(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Create ShipmentContactMech
     */
    public static Map<String, Object> createShipmentContactMech(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue newEntity = delegator.makeValue("ShipmentContactMech");
        newEntity.setPKFields(context);
        newEntity.setNonPKFields(context);
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Update ShipmentContactMech
     */
    public static Map<String, Object> updateShipmentContactMech(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookupPKMap = delegator.makeValue("ShipmentContactMech");
        lookupPKMap.setPKFields(context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ShipmentContactMech")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ShipmentContactMech: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        lookedUpValue.setNonPKFields(context);
        try {
            delegator.store(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Delete ShipmentContactMech
     */
    public static Map<String, Object> deleteShipmentContactMech(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Object fromStatusId = null;
        Map<String, Object> inlineResult = null;
        Object operationName = "Delete ShipmentContactMech";
        inlineResult = checkCanChangeShipmentStatusPacked(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue lookupPKMap = delegator.makeValue("ShipmentContactMech");
        lookupPKMap.setPKFields(context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ShipmentContactMech")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ShipmentContactMech: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            delegator.removeValue(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Create ShipmentRouteSegment
     */
    public static Map<String, Object> createShipmentRouteSegment(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue newEntity = null;
        Object fromStatusId = null;
        Map<String, Object> inlineResult = null;
        GenericValue checkShipmentPackageRouteSeg = null;
        GenericValue shipmentPackage = null;
        Map<String, Object> createShipmentPackageRouteSegMap = null;
        List<GenericValue> shipmentPackages = null;
        Object operationName = "Create ShipmentRouteSegment";
        inlineResult = checkCanChangeShipmentStatusPacked(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        newEntity = delegator.makeValue("ShipmentRouteSegment");
        newEntity.setPKFields(context);
        newEntity.setNonPKFields(context);
        delegator.setNextSubSeqId(newEntity, "shipmentRouteSegmentId", 5, 1);
        Object shipmentRouteSegmentId = newEntity.get("shipmentRouteSegmentId");
        result.put("shipmentRouteSegmentId", newEntity.get("shipmentRouteSegmentId"));
        if (UtilValidate.isEmpty(newEntity.get("carrierServiceStatusId"))) {
            newEntity.put("carrierServiceStatusId", "SHRSCS_NOT_STARTED");
        }
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Object shipmentId = newEntity.get("shipmentId");
        shipmentRouteSegmentId = newEntity.get("shipmentRouteSegmentId");
        inlineResult = ensureRouteSegPackage(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }

        return result;
    }


    /**
     * Update ShipmentRouteSegment
     */
    public static Map<String, Object> updateShipmentRouteSegment(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Map<String, Object> newEntity = null;
        Object fromStatusId = null;
        Map<String, Object> inlineResult = null;
        GenericValue checkShipmentPackageRouteSeg = null;
        GenericValue shipmentPackage = null;
        Map<String, Object> createShipmentPackageRouteSegMap = null;
        List<GenericValue> shipmentPackages = null;
        Object operationName = "Update ShipmentRouteSegment";
        inlineResult = checkCanChangeShipmentStatusDelivered(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ShipmentRouteSegment")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ShipmentRouteSegment: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        lookedUpValue.setNonPKFields(context);
        if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("carrierServiceStatusId"))) {
            newEntity.put("carrierServiceStatusId", "SHRSCS_NOT_STARTED");
        }
        lookedUpValue.put("updatedByUserLoginId", userLogin.get("userLoginId"));
        Timestamp lookedUpValue_lastUpdatedDate = new Timestamp(System.currentTimeMillis());
        try {
            delegator.store(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Object shipmentId = lookedUpValue.get("shipmentId");
        Object shipmentRouteSegmentId = lookedUpValue.get("shipmentRouteSegmentId");
        inlineResult = ensureRouteSegPackage(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }

        return result;
    }


    /**
     * Delete ShipmentRouteSegment
     */
    public static Map<String, Object> deleteShipmentRouteSegment(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Object fromStatusId = null;
        Map<String, Object> inlineResult = null;
        Object operationName = "Delete ShipmentRouteSegment";
        inlineResult = checkCanChangeShipmentStatusPacked(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ShipmentRouteSegment")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ShipmentRouteSegment: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            delegator.removeValue(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Ensure ShipmentPackageRouteSeg exists for all Packages for this RouteSegment
     */
    public static Map<String, Object> ensureRouteSegPackage(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue checkShipmentPackageRouteSeg = null;
        Map<String, Object> createShipmentPackageRouteSegMap = null;
        List<GenericValue> shipmentPackages = null;
        try {
            shipmentPackages = EntityQuery.use(delegator)
                    .from("ShipmentPackage")
                    .where(UtilMisc.toMap("shipmentId", context.get("shipmentId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (shipmentPackages != null) {
            for (GenericValue shipmentPackage : shipmentPackages) {
                try {
                    checkShipmentPackageRouteSeg = EntityQuery.use(delegator)
                            .from("ShipmentPackageRouteSeg")
                            .where(UtilMisc.toMap("shipmentId", context.get("shipmentId"), "shipmentRouteSegmentId", context.get("shipmentRouteSegmentId"), "shipmentPackageSeqId", shipmentPackage.get("shipmentPackageSeqId")))
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying ShipmentPackageRouteSeg: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (UtilValidate.isEmpty(checkShipmentPackageRouteSeg)) {
                    createShipmentPackageRouteSegMap.put("shipmentId", context.get("shipmentId"));
                    createShipmentPackageRouteSegMap.put("shipmentRouteSegmentId", context.get("shipmentRouteSegmentId"));
                    createShipmentPackageRouteSegMap.put("shipmentPackageSeqId", shipmentPackage.get("shipmentPackageSeqId"));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("createShipmentPackageRouteSeg", createShipmentPackageRouteSegMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling createShipmentPackageRouteSeg: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            }
        }

        return result;
    }


    /**
     * Check the Status of a Shipment to see if it can be changed - meant to be called in-line
     */
    public static Map<String, Object> checkCanChangeShipmentStatusPacked(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue testShipment = null;
        GenericValue testShipmentStatus = null;
        List<String> error_list = null;
        Object fromStatusId = "SHIPMENT_PACKED";
        Map<String, Object> inlineResult = checkCanChangeShipmentStatusGeneral(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }

        return result;
    }


    /**
     * Check the Status of a Shipment to see if it can be changed - meant to be called in-line
     */
    public static Map<String, Object> checkCanChangeShipmentStatusShipped(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue testShipment = null;
        GenericValue testShipmentStatus = null;
        List<String> error_list = null;
        Object fromStatusId = "SHIPMENT_SHIPPED";
        Map<String, Object> inlineResult = checkCanChangeShipmentStatusGeneral(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }

        return result;
    }


    /**
     * Check the Status of a Shipment to see if it can be changed - meant to be called in-line
     */
    public static Map<String, Object> checkCanChangeShipmentStatusDelivered(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue testShipment = null;
        GenericValue testShipmentStatus = null;
        List<String> error_list = null;
        Object fromStatusId = "SHIPMENT_DELIVERED";
        Map<String, Object> inlineResult = checkCanChangeShipmentStatusGeneral(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }

        return result;
    }


    /**
     * Check the Status of a Shipment to see if it can be changed - meant to be called in-line
     */
    public static Map<String, Object> checkCanChangeShipmentStatusGeneral(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Object operationName = context.get("operationName");
        GenericValue testShipmentStatus = null;
        List<String> error_list = null;
        GenericValue testShipment = null;
        try {
            testShipment = EntityQuery.use(delegator)
                    .from("Shipment")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Shipment: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if ((((UtilValidate.isEmpty(context.get("fromStatusId")) || "SHIPMENT_PACKED".equals(context.get("fromStatusId"))) && "SHIPMENT_PACKED".equals(testShipment.get("statusId"))) || (("SHIPMENT_PACKED".equals(context.get("fromStatusId")) || "SHIPMENT_SHIPPED".equals(context.get("fromStatusId"))) && "SHIPMENT_SHIPPED".equals(testShipment.get("statusId"))) || (("SHIPMENT_PACKED".equals(context.get("fromStatusId")) || "SHIPMENT_SHIPPED".equals(context.get("fromStatusId")) || "SHIPMENT_DELIVERED".equals(context.get("fromStatusId"))) && "SHIPMENT_DELIVERED".equals(testShipment.get("statusId"))) || "SHIPMENT_CANCELLED".equals(testShipment.get("statusId")))) {
            try {
                testShipmentStatus = testShipment.getRelatedOne("StatusItem", false);
            } catch (Exception e) {
                Debug.logError(e, "Error getting related one StatusItem: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            error_list.add("Cannot perform operation " + operationName + " when the shipment is in the " + testShipmentStatus.get("description") + " [" + testShipment.get("statusId") + "] status.");
        }

        return result;
    }


    /**
     * Creates a CarrierShipmentMethod
     */
    public static Map<String, Object> createCarrierShipmentMethod(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue carrierShipmentMethod = delegator.makeValue("CarrierShipmentMethod");
        carrierShipmentMethod.setPKFields(context);
        carrierShipmentMethod.setNonPKFields(context);
        try {
            delegator.create(carrierShipmentMethod);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Updates a CarrierShipmentMethod
     */
    public static Map<String, Object> updateCarrierShipmentMethod(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue carrierShipmentMethod = null;
        try {
            carrierShipmentMethod = EntityQuery.use(delegator)
                    .from("CarrierShipmentMethod")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying CarrierShipmentMethod: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        carrierShipmentMethod.setNonPKFields(context);
        try {
            delegator.store(carrierShipmentMethod);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Removes a CarrierShipmentMethod
     */
    public static Map<String, Object> deleteCarrierShipmentMethod(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue carrierShipmentMethod = null;
        try {
            carrierShipmentMethod = EntityQuery.use(delegator)
                    .from("CarrierShipmentMethod")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying CarrierShipmentMethod: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            delegator.removeValue(carrierShipmentMethod);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Creates a ShipmentMethodType
     */
    public static Map<String, Object> createShipmentMethodType(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue shipmentMethodType = delegator.makeValue("ShipmentMethodType");
        shipmentMethodType.setPKFields(context);
        shipmentMethodType.setNonPKFields(context);
        try {
            delegator.create(shipmentMethodType);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Updates a ShipmentMethodType
     */
    public static Map<String, Object> updateShipmentMethodType(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue shipmentMethodType = null;
        try {
            shipmentMethodType = EntityQuery.use(delegator)
                    .from("ShipmentMethodType")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ShipmentMethodType: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        shipmentMethodType.setNonPKFields(context);
        try {
            delegator.store(shipmentMethodType);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Deletes a ShipmentMethodType
     */
    public static Map<String, Object> deleteShipmentMethodType(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue shipmentMethodType = null;
        try {
            shipmentMethodType = EntityQuery.use(delegator)
                    .from("ShipmentMethodType")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ShipmentMethodType: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            delegator.removeValue(shipmentMethodType);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Quick ships an entire order from multiple facilities
     */
    public static Map<String, Object> quickShipEntireOrder(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        List<Object> orderItemShipGrpInvResFacilityIds = null;
        Object setPackedOnly = null;
        GenericValue facility = null;
        Map<String, Object> inlineResult = null;
        Object eventDate = null;
        List<GenericValue> orderItemAndShipGroupAssocList = null;
        List<Object> orderItemListByShGrpMap_orderItemAndShipGroupAssoc_shipGroupSeqId_ = null;
        List<GenericValue> orderItemShipGroupList = null;
        GenericValue orderItemAndShipGroupAssoc = null;
        Object partyIdFrom = null;
        Object itemResFindMap = null;
        GenericValue orderItemShipGroup = null;
        Map<String, Object> shipmentShipGroupFacility = null;
        Object shipmentPackageSeqId = null;
        Object perShipGroupItemList = null;
        GenericValue orderRole = null;
        List<GenericValue> orderItems = null;
        Map<String, Object> shipmentLookupMap = null;
        List<GenericValue> itemResList = null;
        List<GenericValue> orderRoles = null;
        GenericValue itemIssuance = null;
        List<GenericValue> itemIssuances = null;
        GenericValue item = null;
        List<Object> argListNames = null;
        GenericValue shipment = null;
        List<Object> shipmentShipGroupFacilityList = null;
        String successMessage = null;
        Object shipItemContext = null;
        Map<String, Object> shipmentContext = null;
        GenericValue itemRes = null;
        Map<String, Object> issueContext = null;
        Map<String, Object> packedContext = null;
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
        if (UtilValidate.isEmpty(orderHeader.get("productStoreId"))) {
            {
                String errorMsg = UtilProperties.getMessage("ProductUiLabels", "FacilityShipmentMissingProductStore", locale);
                error_list.add(errorMsg);
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
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
        if (!"Y".equals(productStore.get("reserveInventory"))) {
            {
                String errorMsg = UtilProperties.getMessage("ProductUiLabels", "FacilityShipmentNotCreatedForNotReserveInventory", locale);
                error_list.add(errorMsg);
            }
        }
        if ("Y".equals(productStore.get("explodeOrderItems"))) {
            {
                String errorMsg = UtilProperties.getMessage("ProductUiLabels", "FacilityShipmentNotCreatedForExplodesOrderItems", locale);
                error_list.add(errorMsg);
            }
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        List<GenericValue> orderItemAndShipGrpInvResAndItemList = null;
        try {
            orderItemAndShipGrpInvResAndItemList = EntityQuery.use(delegator)
                    .from("OrderItemAndShipGrpInvResAndItem")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderItemAndShipGrpInvResAndItem: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (orderItemAndShipGrpInvResAndItemList != null) {
            for (GenericValue orderItemAndShipGrpInvResAndItem : orderItemAndShipGrpInvResAndItemList) {
                if (!(orderItemShipGrpInvResFacilityIds != null /* TODO: field compare operator contains */)) {
                    orderItemShipGrpInvResFacilityIds.add(orderItemAndShipGrpInvResAndItem.get("facilityId"));
                }
            }
        }
        inlineResult = getOrderItemShipGroupLists(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (orderItemShipGrpInvResFacilityIds != null) {
            for (Object orderItemShipGrpInvResFacilityId : orderItemShipGrpInvResFacilityIds) {
                try {
                    facility = EntityQuery.use(delegator)
                            .from("Facility")
                            .where(UtilMisc.toMap("facilityId", orderItemShipGrpInvResFacilityId))
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying Facility: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                eventDate = context.get("eventDate");
                setPackedOnly = context.get("setPackedOnly");
                inlineResult = createShipmentForFacilityAndShipGroup(dctx, context);
                if (ServiceUtil.isError(inlineResult)) {
                    return inlineResult;
                }
            }
        }
        Debug.logInfo("Finished quickShipEntireOrder:\\nshipmentShipGroupFacilityList=" + shipmentShipGroupFacilityList + "\\nsuccessMessageList=" + context.get("successMessageList"), MODULE);
        result.put("shipmentShipGroupFacilityList", shipmentShipGroupFacilityList);
        result.put("successMessageList", context.get("successMessageList"));
        if (UtilValidate.isEmpty(shipmentShipGroupFacilityList)) {
            {
                String errorMsg = UtilProperties.getMessage("ProductUiLabels", "FacilityShipmentNotCreated", locale);
                error_list.add(errorMsg);
            }
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }

        return result;
    }


    /**
     * Create and complete a drop shipment for a ship group
     */
    public static Map<String, Object> quickDropShipOrder(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Map<String, Object> itemStatusContext = null;
        GenericValue orderItem = null;
        List<GenericValue> orderItemAssocs = null;
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
        if ("ORDER_CREATED".equals(orderHeader.get("statusId"))) {
            {
                String errorMsg = UtilProperties.getMessage("OrderErrorUiLabels", "OrderApproveOrderBeforeQuickDropShip", locale);
                error_list.add(errorMsg);
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        }
        Map<String, Object> shipmentContext = new HashMap<String, Object>();
        shipmentContext.put("primaryOrderId", context.get("orderId"));
        shipmentContext.put("primaryShipGroupSeqId", context.get("shipGroupSeqId"));
        shipmentContext.put("statusId", "PURCH_SHIP_CREATED");
        shipmentContext.put("shipmentTypeId", "DROP_SHIPMENT");
        Object shipmentId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createShipment", shipmentContext);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            shipmentId = serviceResult.get("shipmentId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createShipment: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        Map<String, Object> updateShipmentContext = new HashMap<String, Object>();
        updateShipmentContext.put("shipmentId", shipmentId);
        updateShipmentContext.put("statusId", "PURCH_SHIP_SHIPPED");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateShipment", updateShipmentContext);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateShipment: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        updateShipmentContext.put("statusId", "PURCH_SHIP_RECEIVED");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateShipment", updateShipmentContext);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateShipment: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        result.put("shipmentId", shipmentId);
        List<GenericValue> orderItemShipGroupAssocs = null;
        try {
            orderItemShipGroupAssocs = EntityQuery.use(delegator)
                    .from("OrderItemShipGroupAssoc")
                    .where(UtilMisc.toMap("orderId", context.get("orderId"), "shipGroupSeqId", context.get("shipGroupSeqId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (orderItemShipGroupAssocs != null) {
            for (GenericValue orderItemShipGroupAssoc : orderItemShipGroupAssocs) {
                try {
                    orderItem = orderItemShipGroupAssoc.getRelatedOne("OrderItem", false);
                } catch (Exception e) {
                    Debug.logError(e, "Error getting related one OrderItem: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                itemStatusContext.put("orderId", context.get("orderId"));
                itemStatusContext.put("orderItemSeqId", orderItem.get("orderItemSeqId"));
                itemStatusContext.put("statusId", "ITEM_COMPLETED");
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("changeOrderItemStatus", itemStatusContext);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling changeOrderItemStatus: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (!error_list.isEmpty()) {
                    return ServiceUtil.returnError(error_list);
                }
                try {
                    orderItemAssocs = EntityQuery.use(delegator)
                            .from("OrderItemAssoc")
                            .where(UtilMisc.toMap("toOrderId", context.get("orderId"), "toOrderItemSeqId", orderItem.get("orderItemSeqId"), "orderItemAssocTypeId", "DROP_SHIPMENT"))
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (UtilValidate.isNotEmpty(orderItemAssocs)) {
                    if (orderItemAssocs != null) {
                        for (GenericValue orderItemAssoc : orderItemAssocs) {
                            itemStatusContext.put("orderId", orderItemAssoc.get("orderId"));
                            itemStatusContext.put("orderItemSeqId", orderItemAssoc.get("orderItemSeqId"));
                            try {
                                Map<String, Object> serviceResult = dispatcher.runSync("changeOrderItemStatus", itemStatusContext);
                                if (ServiceUtil.isError(serviceResult)) {
                                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                                }
                            } catch (Exception e) {
                                Debug.logError(e, "Error calling changeOrderItemStatus: " + e.getMessage(), MODULE);
                                return ServiceUtil.returnError(e.getMessage());
                            }
                            if (!error_list.isEmpty()) {
                                return ServiceUtil.returnError(error_list);
                            }
                        }
                    }
                }
            }
        }

        return result;
    }


    /**
     * Quick ships an entire purchase order to a facility
     */
    public static Map<String, Object> quickShipPurchaseOrder(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<GenericValue> orderItemAndShipGroupAssocList = null;
        List<Object> orderItemListByShGrpMap_orderItemAndShipGroupAssoc_shipGroupSeqId_ = null;
        List<GenericValue> orderItemShipGroupList = null;
        GenericValue orderItemAndShipGroupAssoc = null;
        Object partyIdFrom = null;
        Object itemResFindMap = null;
        GenericValue orderItemShipGroup = null;
        Map<String, Object> shipmentShipGroupFacility = null;
        Object shipmentPackageSeqId = null;
        Object perShipGroupItemList = null;
        GenericValue orderRole = null;
        List<GenericValue> orderItems = null;
        Map<String, Object> shipmentLookupMap = null;
        List<GenericValue> itemResList = null;
        List<GenericValue> orderRoles = null;
        GenericValue itemIssuance = null;
        List<GenericValue> itemIssuances = null;
        GenericValue item = null;
        List<Object> argListNames = null;
        GenericValue shipment = null;
        List<Object> shipmentShipGroupFacilityList = null;
        String successMessage = null;
        Object shipItemContext = null;
        Map<String, Object> shipmentContext = null;
        GenericValue itemRes = null;
        Map<String, Object> issueContext = null;
        Map<String, Object> packedContext = null;
        GenericValue facility = null;
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
        try {
            facility = EntityQuery.use(delegator)
                    .from("Facility")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Facility: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> inlineResult = getOrderItemShipGroupLists(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        inlineResult = createShipmentForFacilityAndShipGroup(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        Debug.logInfo("Finished quickShipPurchaseOrder for orderId " + context.get("orderId") + " and destination facilityId " + context.get("facilityId"), MODULE);

        return result;
    }


    /**
     * Sub-method used by quickShip methods to get a list of OrderItemAndShipGroupAssoc and a Map of shipGroupId -&gt; OrderItemAndShipGroupAssoc
     */
    public static Map<String, Object> getOrderItemShipGroupLists(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        List<Object> orderItemListByShGrpMap_orderItemAndShipGroupAssoc_shipGroupSeqId_ = null;
        List<GenericValue> orderItemAndShipGroupAssocList = null;
        try {
            orderItemAndShipGroupAssocList = EntityQuery.use(delegator)
                    .from("OrderItemAndShipGroupAssoc")
                    .where(UtilMisc.toMap("orderId", ((Map<String, Object>) context.get("orderHeader")).get("orderId"), "statusId", "ITEM_APPROVED"))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(orderItemAndShipGroupAssocList)) {
            {
                String errorMsg = UtilProperties.getMessage("ProductUiLabels", "FacilityNoItemsAvailableToShip", locale);
                error_list.add(errorMsg);
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        }
        List<GenericValue> orderItemShipGroupList = null;
        try {
            orderItemShipGroupList = ((GenericValue) context.get("orderHeader")).getRelated("OrderItemShipGroup", null, null, false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related OrderItemShipGroup: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (orderItemAndShipGroupAssocList != null) {
            for (GenericValue orderItemAndShipGroupAssoc : orderItemAndShipGroupAssocList) {
                orderItemListByShGrpMap_orderItemAndShipGroupAssoc_shipGroupSeqId_.add(orderItemAndShipGroupAssoc);
            }
        }

        return result;
    }


    /**
     * Sub-method used by quickShip methods to create a shipment
     */
    public static Map<String, Object> createShipmentForFacilityAndShipGroup(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Object orderItemShipGroupList = context.get("orderItemShipGroupList");
        Object partyIdFrom = null;
        Object itemResFindMap = null;
        Map<String, Object> shipmentShipGroupFacility = null;
        Object shipmentPackageSeqId = null;
        Object perShipGroupItemList = null;
        GenericValue orderRole = null;
        List<GenericValue> orderItems = null;
        Map<String, Object> shipmentLookupMap = null;
        List<GenericValue> itemResList = null;
        List<GenericValue> orderRoles = null;
        List<GenericValue> itemIssuances = null;
        List<Object> argListNames = null;
        GenericValue shipment = null;
        List<Object> shipmentShipGroupFacilityList = null;
        String successMessage = null;
        Object shipItemContext = null;
        Map<String, Object> shipmentContext = null;
        Map<String, Object> issueContext = null;
        Map<String, Object> packedContext = null;
        GenericValue facility = null;
        Debug.logInfo("orderHeader.orderId: " + ((Map<String, Object>) context.get("orderHeader")).get("orderId"), MODULE);
        Debug.logInfo("orderItemShipGroupList: " + orderItemShipGroupList, MODULE);
        if (orderItemShipGroupList != null) {
            for (Object orderItemShipGroup : (List<?>) orderItemShipGroupList) {
                try {
                    orderItems = EntityQuery.use(delegator)
                            .from("OrderItemAndShipGroupAssoc")
                            .where(UtilMisc.toMap("orderId", ((Map<String, Object>) context.get("orderHeader")).get("orderId"), "shipGroupSeqId", ((Map<String, Object>) orderItemShipGroup).get("shipGroupSeqId"), "statusId", "ITEM_APPROVED"))
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                perShipGroupItemList = ((Map<String, Object>) context.get("orderItemListByShGrpMap")).get(((Map<String, Object>) orderItemShipGroup).get("shipGroupSeqId"));
                Debug.logInfo("perShipGroupItemList: " + perShipGroupItemList, MODULE);
                GenericValue item = null;
                GenericValue orderItemAndShipGroupAssoc = null;
                GenericValue itemRes = null;
                GenericValue itemIssuance = null;
                if (UtilValidate.isEmpty(perShipGroupItemList)) {
                    argListNames.add(((Map<String, Object>) orderItemShipGroup).get("shipGroupSeqId"));
                    successMessage = UtilProperties.getMessage("ProductUiLabels", "FacilityShipmentNoItemsAvailableToShip", locale);
                } else {
                    shipmentContext.put("primaryOrderId", ((Map<String, Object>) context.get("orderHeader")).get("orderId"));
                    shipmentContext.put("primaryShipGroupSeqId", ((Map<String, Object>) orderItemShipGroup).get("shipGroupSeqId"));
                    Map<String, Object> orderHeader = new HashMap<String, Object>();
                    Object shipmentContext_partyIdFrom = null;
                    Object shipmentContext_originFacilityId = null;
                    Object shipmentContext_statusId = null;
                    Object shipmentContext_destinationFacilityId = null;
                    if ("SALES_ORDER".equals(((Map<String, Object>) orderHeader).get("orderTypeId"))) {
                        if (UtilValidate.isNotEmpty(((Map<String, Object>) orderItemShipGroup).get("vendorPartyId"))) {
                            partyIdFrom = ((Map<String, Object>) orderItemShipGroup).get("vendorPartyId");
                        } else {
                            try {
                                facility = EntityQuery.use(delegator)
                                        .from("Facility")
                                        .where(UtilMisc.toMap("facilityId", context.get("orderItemShipGrpInvResFacilityId")))
                                        .queryOne();
                            } catch (Exception e) {
                                Debug.logError(e, "Error querying Facility: " + e.getMessage(), MODULE);
                                return ServiceUtil.returnError(e.getMessage());
                            }
                            if (UtilValidate.isNotEmpty(facility.get("ownerPartyId"))) {
                                partyIdFrom = facility.get("ownerPartyId");
                            }
                            if (UtilValidate.isEmpty(partyIdFrom)) {
                                try {
                                    orderRoles = EntityQuery.use(delegator)
                                            .from("OrderRole")
                                            .where(UtilMisc.toMap("orderId", ((Map<String, Object>) orderHeader).get("orderId"), "roleTypeId", "SHIP_FROM_VENDOR"))
                                            .queryList();
                                } catch (Exception e) {
                                    Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                                    return ServiceUtil.returnError(e.getMessage());
                                }
                                if (UtilValidate.isNotEmpty(orderRoles)) {
                                    orderRole = EntityUtil.getFirst((List<GenericValue>) orderRoles);
                                    partyIdFrom = orderRole.get("partyId");
                                } else {
                                    try {
                                        orderRoles = EntityQuery.use(delegator)
                                                .from("OrderRole")
                                                .where(UtilMisc.toMap("orderId", ((Map<String, Object>) orderHeader).get("orderId"), "roleTypeId", "BILL_FROM_VENDOR"))
                                                .queryList();
                                    } catch (Exception e) {
                                        Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                                        return ServiceUtil.returnError(e.getMessage());
                                    }
                                    orderRole = EntityUtil.getFirst((List<GenericValue>) orderRoles);
                                    partyIdFrom = orderRole.get("partyId");
                                }
                            }
                        }
                        shipmentContext.put("partyIdFrom", partyIdFrom);
                        shipmentContext.put("originFacilityId", context.get("orderItemShipGrpInvResFacilityId"));
                        shipmentContext.put("statusId", "SHIPMENT_INPUT");
                    } else {
                        shipmentContext.put("destinationFacilityId", facility.get("facilityId"));
                        shipmentContext.put("statusId", "PURCH_SHIP_CREATED");
                    }
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("createShipment", shipmentContext);
                        if (ServiceUtil.isError(serviceResult)) {
                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                        }
                        shipmentLookupMap.put("shipmentId", serviceResult.get("shipmentId"));
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling createShipment: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    try {
                        shipment = EntityQuery.use(delegator)
                                .from("Shipment")
                                .where(shipmentLookupMap)
                                .queryOne();
                    } catch (Exception e) {
                        Debug.logError(e, "Error finding by primary key Shipment: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    Object itemResFindMap_facilityId = null;
                    Object issueContext_shipmentId = null;
                    Object issueContext_orderId = null;
                    Object issueContext_orderItemSeqId = null;
                    Object issueContext_shipGroupSeqId = null;
                    Object issueContext_inventoryItemId = null;
                    Object issueContext_quantity = null;
                    Object issueContext_eventDate = null;
                    if ("SALES_ORDER".equals(((Map<String, Object>) orderHeader).get("orderTypeId"))) {
                        if (perShipGroupItemList != null) {
                            for (Object orderItemAndShipGroupAssocEntry : (List<?>) perShipGroupItemList) {
                                itemResFindMap = new HashMap<String, Object>();
                                ((Map<String, Object>) itemResFindMap).put("facilityId", context.get("orderItemShipGrpInvResFacilityId"));
                                try {
                                    itemResList = ((GenericValue) orderItemAndShipGroupAssocEntry).getRelated("OrderItemShipGrpInvResAndItem", null, null, false);
                                } catch (Exception e) {
                                    Debug.logError(e, "Error getting related OrderItemShipGrpInvResAndItem: " + e.getMessage(), MODULE);
                                    return ServiceUtil.returnError(e.getMessage());
                                }
                                if (itemResList != null) {
                                    for (GenericValue itemResEntry : itemResList) {
                                        issueContext.put("shipmentId", shipment.get("shipmentId"));
                                        issueContext.put("orderId", itemResEntry.get("orderId"));
                                        issueContext.put("orderItemSeqId", itemResEntry.get("orderItemSeqId"));
                                        issueContext.put("shipGroupSeqId", itemResEntry.get("shipGroupSeqId"));
                                        issueContext.put("inventoryItemId", itemResEntry.get("inventoryItemId"));
                                        issueContext.put("quantity", itemResEntry.get("quantity"));
                                        issueContext.put("eventDate", context.get("eventDate"));
                                        try {
                                            Map<String, Object> serviceResult = dispatcher.runSync("issueOrderItemShipGrpInvResToShipment", issueContext);
                                            if (ServiceUtil.isError(serviceResult)) {
                                                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                                            }
                                        } catch (Exception e) {
                                            Debug.logError(e, "Error calling issueOrderItemShipGrpInvResToShipment: " + e.getMessage(), MODULE);
                                            return ServiceUtil.returnError(e.getMessage());
                                        }
                                    }
                                }
                            }
                        }
                    } else {
                        itemResFindMap = new HashMap<String, Object>();
                        ((Map<String, Object>) itemResFindMap).put("facilityId", context.get("facilityId"));
                        if (context.get("orderItemAndShipGroupAssocList") != null) {
                            for (Object itemEntry : (List<?>) context.get("orderItemAndShipGroupAssocList")) {
                                issueContext.put("shipmentId", shipment.get("shipmentId"));
                                issueContext.put("orderId", ((Map<String, Object>) itemEntry).get("orderId"));
                                issueContext.put("orderItemSeqId", ((Map<String, Object>) itemEntry).get("orderItemSeqId"));
                                issueContext.put("shipGroupSeqId", ((Map<String, Object>) itemEntry).get("shipGroupSeqId"));
                                issueContext.put("quantity", ((Map<String, Object>) itemEntry).get("quantity"));
                                try {
                                    Map<String, Object> serviceResult = dispatcher.runSync("issueOrderItemToShipment", issueContext);
                                    if (ServiceUtil.isError(serviceResult)) {
                                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                                    }
                                } catch (Exception e) {
                                    Debug.logError(e, "Error calling issueOrderItemToShipment: " + e.getMessage(), MODULE);
                                    return ServiceUtil.returnError(e.getMessage());
                                }
                            }
                        }
                    }
                    try {
                        itemIssuances = EntityQuery.use(delegator)
                                .from("ItemIssuance")
                                .where(UtilMisc.toMap("orderId", ((Map<String, Object>) orderHeader).get("orderId"), "shipGroupSeqId", ((Map<String, Object>) orderItemShipGroup).get("shipGroupSeqId"), "shipmentId", shipment.get("shipmentId")))
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    shipmentPackageSeqId = "New";
                    if (itemIssuances != null) {
                        for (GenericValue itemIssuanceEntry : itemIssuances) {
                            Debug.logVerbose("In quick ship adding item to package: " + shipmentPackageSeqId, MODULE);
                            shipItemContext = new HashMap<String, Object>();
                            ((Map<String, Object>) shipItemContext).put("shipmentId", itemIssuanceEntry.get("shipmentId"));
                            ((Map<String, Object>) shipItemContext).put("shipmentItemSeqId", itemIssuanceEntry.get("shipmentItemSeqId"));
                            ((Map<String, Object>) shipItemContext).put("quantity", itemIssuanceEntry.get("quantity"));
                            ((Map<String, Object>) shipItemContext).put("shipmentPackageSeqId", shipmentPackageSeqId);
                            try {
                                Map<String, Object> serviceResult = dispatcher.runSync("addShipmentContentToPackage", (Map<String, Object>) shipItemContext);
                                if (ServiceUtil.isError(serviceResult)) {
                                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                                }
                                shipmentPackageSeqId = serviceResult.get("shipmentPackageSeqId");
                            } catch (Exception e) {
                                Debug.logError(e, "Error calling addShipmentContentToPackage: " + e.getMessage(), MODULE);
                                return ServiceUtil.returnError(e.getMessage());
                            }
                        }
                    }
                    Object packedContext_shipmentId = null;
                    Object packedContext_eventDate = null;
                    Object packedContext_statusId = null;
                    if ("SALES_ORDER".equals(((Map<String, Object>) orderHeader).get("orderTypeId"))) {
                        packedContext.put("shipmentId", shipment.get("shipmentId"));
                        packedContext.put("eventDate", context.get("eventDate"));
                        packedContext.put("statusId", "SHIPMENT_PACKED");
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("updateShipment", packedContext);
                            if (ServiceUtil.isError(serviceResult)) {
                                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                            }
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling updateShipment: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                        if (UtilValidate.isEmpty(context.get("setPackedOnly"))) {
                            packedContext.put("shipmentId", shipment.get("shipmentId"));
                            packedContext.put("statusId", "SHIPMENT_SHIPPED");
                            try {
                                Map<String, Object> serviceResult = dispatcher.runSync("updateShipment", packedContext);
                                if (ServiceUtil.isError(serviceResult)) {
                                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                                }
                            } catch (Exception e) {
                                Debug.logError(e, "Error calling updateShipment: " + e.getMessage(), MODULE);
                                return ServiceUtil.returnError(e.getMessage());
                            }
                        }
                    } else {
                        packedContext.put("shipmentId", shipment.get("shipmentId"));
                        packedContext.put("statusId", "PURCH_SHIP_SHIPPED");
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("updateShipment", packedContext);
                            if (ServiceUtil.isError(serviceResult)) {
                                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                            }
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling updateShipment: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                    }
                    shipmentShipGroupFacility.put("shipmentId", shipment.get("shipmentId"));
                    shipmentShipGroupFacility.put("facilityId", facility.get("facilityId"));
                    shipmentShipGroupFacility.put("shipGroupSeqId", ((Map<String, Object>) orderItemShipGroup).get("shipGroupSeqId"));
                    shipmentShipGroupFacilityList.add(shipmentShipGroupFacility);
                    argListNames.add(((Map<String, Object>) shipmentShipGroupFacility).get("shipmentId"));
                    argListNames.add(((Map<String, Object>) shipmentShipGroupFacility).get("shipGroupSeqId"));
                    argListNames.add(((Map<String, Object>) shipmentShipGroupFacility).get("facilityId"));
                    successMessage = UtilProperties.getMessage("ProductUiLabels", "FacilityShipmentIdCreated", locale);
                    shipmentShipGroupFacility = new HashMap<String, Object>();
                }
            }
        }

        return result;
    }


    /**
     * Create Shipment, ShipmentItems and OrderShipment
     */
    public static Map<String, Object> createOrderShipmentPlan(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Object createShipmentContext = null;
        GenericValue itemProductType = null;
        GenericValue shipment = null;
        GenericValue itemProduct = null;
        Object addOrderShipmentToShipmentCtx = null;
        List<GenericValue> orderItems = null;
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
        if (UtilValidate.isEmpty(orderHeader.get("productStoreId"))) {
            {
                String errorMsg = UtilProperties.getMessage("ProductUiLabels", "FacilityNoQuickShip", locale);
                error_list.add(errorMsg);
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
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
        List<GenericValue> orderItemShipGroupList = null;
        try {
            orderItemShipGroupList = orderHeader.getRelated("OrderItemShipGroup", null, null, false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related OrderItemShipGroup: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (orderItemShipGroupList != null) {
            for (GenericValue orderItemShipGroup : orderItemShipGroupList) {
                createShipmentContext = new HashMap<String, Object>();
                ((Map<String, Object>) createShipmentContext).put("primaryOrderId", orderHeader.get("orderId"));
                ((Map<String, Object>) createShipmentContext).put("primaryShipGroupSeqId", orderItemShipGroup.get("shipGroupSeqId"));
                ((Map<String, Object>) createShipmentContext).put("statusId", "SHIPMENT_INPUT");
                ((Map<String, Object>) createShipmentContext).put("originFacilityId", productStore.get("inventoryFacilityId"));
                ((Map<String, Object>) createShipmentContext).put("userLogin", context.get("userLogin"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createShipment", (Map<String, Object>) createShipmentContext);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                    context.put("shipmentId", serviceResult.get("shipmentId"));
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createShipment: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                try {
                    shipment = EntityQuery.use(delegator)
                            .from("Shipment")
                            .where(context)
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying Shipment: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                try {
                    orderItems = orderHeader.getRelated("OrderItem", null, null, false);
                } catch (Exception e) {
                    Debug.logError(e, "Error getting related OrderItem: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (orderItems != null) {
                    for (GenericValue orderItem : orderItems) {
                        try {
                            itemProduct = EntityQuery.use(delegator)
                                    .from("Product")
                                    .where(UtilMisc.toMap("productId", orderItem.get("productId")))
                                    .cache()
                                    .queryOne();
                        } catch (Exception e) {
                            Debug.logError(e, "Error querying Product: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                        if (UtilValidate.isNotEmpty(itemProduct)) {
                            try {
                                itemProductType = itemProduct.getRelatedOne("ProductType", true);
                            } catch (Exception e) {
                                Debug.logError(e, "Error getting related one ProductType: " + e.getMessage(), MODULE);
                                return ServiceUtil.returnError(e.getMessage());
                            }
                            if ("Y".equals(itemProductType.get("isPhysical"))) {
                                addOrderShipmentToShipmentCtx = new HashMap<String, Object>();
                                ((Map<String, Object>) addOrderShipmentToShipmentCtx).put("orderId", orderHeader.get("orderId"));
                                ((Map<String, Object>) addOrderShipmentToShipmentCtx).put("orderItemSeqId", orderItem.get("orderItemSeqId"));
                                ((Map<String, Object>) addOrderShipmentToShipmentCtx).put("shipmentId", context.get("shipmentId"));
                                ((Map<String, Object>) addOrderShipmentToShipmentCtx).put("quantity", orderItem.get("quantity"));
                                ((Map<String, Object>) addOrderShipmentToShipmentCtx).put("userLogin", context.get("userLogin"));
                                try {
                                    Map<String, Object> serviceResult = dispatcher.runSync("addOrderShipmentToShipment", (Map<String, Object>) addOrderShipmentToShipmentCtx);
                                    if (ServiceUtil.isError(serviceResult)) {
                                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                                    }
                                } catch (Exception e) {
                                    Debug.logError(e, "Error calling addOrderShipmentToShipment: " + e.getMessage(), MODULE);
                                    return ServiceUtil.returnError(e.getMessage());
                                }
                            }
                        }
                    }
                }
                result.put("shipmentId", context.get("shipmentId"));
            }
        }

        return result;
    }


    /**
     */
    public static Map<String, Object> issueSerializedInvToShipmentPackageAndSetTracking(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue inventoryItem = null;
        GenericValue orderItemShipGrpInvResLookupPk = null;
        GenericValue orderItemShipGrpInvRes = null;
        Map<String, Object> reserveAnInventoryItemCtx = new HashMap<>();
        Map<String, Object> shipItemContext = null;
        Map<String, Object> shipPackageContext = null;
        Map<String, Object> routeSegLookup = null;
        GenericValue packageRouteSegment = null;
        if (UtilValidate.isNotEmpty(context.get("serialNumber"))) {
            orderItemShipGrpInvResLookupPk = delegator.makeValue("OrderItemShipGrpInvRes");
            orderItemShipGrpInvResLookupPk.setPKFields(context);
            try {
                orderItemShipGrpInvRes = EntityQuery.use(delegator)
                        .from("OrderItemShipGrpInvRes")
                        .where(orderItemShipGrpInvResLookupPk)
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error finding by primary key OrderItemShipGrpInvRes: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            try {
                inventoryItem = orderItemShipGrpInvRes.getRelatedOne("InventoryItem", false);
            } catch (Exception e) {
                Debug.logError(e, "Error getting related one InventoryItem: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (!java.util.Objects.equals(inventoryItem.get("serialNumber"), context.get("serialNumber"))) {
                // set-service-fields from "parameters" to "reserveAnInventoryItemCtx" for service "reserveAnInventoryItem"
                reserveAnInventoryItemCtx.putAll(UtilMisc.toMap(context));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("reserveAnInventoryItem", reserveAnInventoryItemCtx);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                    context.put("inventoryItemId", serviceResult.get("inventoryItemId"));
                } catch (Exception e) {
                    Debug.logError(e, "Error calling reserveAnInventoryItem: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }
        Object issueContext = null;
        ((Map<String, Object>) issueContext).put("shipmentId", context.get("shipmentId"));
        ((Map<String, Object>) issueContext).put("inventoryItemId", context.get("inventoryItemId"));
        ((Map<String, Object>) issueContext).put("orderId", context.get("orderId"));
        ((Map<String, Object>) issueContext).put("shipGroupSeqId", context.get("shipGroupSeqId"));
        ((Map<String, Object>) issueContext).put("orderItemSeqId", context.get("orderItemSeqId"));
        ((Map<String, Object>) issueContext).put("inventoryItemId", context.get("inventoryItemId"));
        ((Map<String, Object>) issueContext).put("quantity", context.get("quantity"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("issueOrderItemShipGrpInvResToShipment", (Map<String, Object>) issueContext);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            context.put("itemIssuanceId", serviceResult.get("itemIssuanceId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling issueOrderItemShipGrpInvResToShipment: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Debug.logInfo("QuickShipOrderByItem grouping by tracking number : " + context.get("trackingNum"), MODULE);
        GenericValue itemIssuance = null;
        try {
            itemIssuance = EntityQuery.use(delegator)
                    .from("ItemIssuance")
                    .where(UtilMisc.toMap("itemIssuanceId", context.get("itemIssuanceId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ItemIssuance: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        shipItemContext = new HashMap<String, Object>();
        shipItemContext.put("shipmentPackageSeqId", context.get("shipmentPackageSeqId"));
        if (UtilValidate.isEmpty(((Map<String, Object>) shipItemContext).get("shipmentPackageSeqId"))) {
            shipItemContext.put("shipmentPackageSeqId", "New");
        }
        Debug.logInfo("Package SeqID : " + ((Map<String, Object>) shipItemContext).get("shipmentPackageSeqId"), MODULE);
        GenericValue shipmentPackageLookupPk = delegator.makeValue("ShipmentPackage");
        shipmentPackageLookupPk.setPKFields(context);
        GenericValue shipmentPackage = null;
        try {
            shipmentPackage = EntityQuery.use(delegator)
                    .from("ShipmentPackage")
                    .where(shipmentPackageLookupPk)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ShipmentPackage: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(shipmentPackage)) {
            shipPackageContext.put("shipmentId", itemIssuance.get("shipmentId"));
            shipPackageContext.put("shipmentPackageSeqId", ((Map<String, Object>) shipItemContext).get("shipmentPackageSeqId"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createShipmentPackage", shipPackageContext);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling createShipmentPackage: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        shipItemContext.put("shipmentId", itemIssuance.get("shipmentId"));
        shipItemContext.put("shipmentItemSeqId", itemIssuance.get("shipmentItemSeqId"));
        shipItemContext.put("quantity", itemIssuance.get("quantity"));
        Map<String, Object> packageMap = new HashMap<String, Object>();
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("addShipmentContentToPackage", shipItemContext);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            packageMap.put("${parameters.trackingNum}", serviceResult.get("shipmentPackageSeqId"));
            routeSegLookup.put("shipmentPackageSeqId", serviceResult.get("shipmentPackageSeqId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling addShipmentContentToPackage: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(((Map<String, Object>) routeSegLookup).get("shipmentPackageSeqId"))) {
            routeSegLookup.put("shipmentId", itemIssuance.get("shipmentId"));
            routeSegLookup.put("shipmentRouteSegmentId", "00001");
            try {
                packageRouteSegment = EntityQuery.use(delegator)
                        .from("ShipmentPackageRouteSeg")
                        .where(routeSegLookup)
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error finding by primary key ShipmentPackageRouteSeg: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (UtilValidate.isNotEmpty(packageRouteSegment)) {
                packageRouteSegment.put("trackingCode", context.get("trackingNum"));
                try {
                    delegator.store(packageRouteSegment);
                } catch (Exception e) {
                    Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
            if (UtilValidate.isEmpty(packageRouteSegment)) {
                Debug.logWarning("No route segment found : " + routeSegLookup, MODULE);
            }
        }
        if (UtilValidate.isEmpty(((Map<String, Object>) routeSegLookup).get("shipmentPackageSeqId"))) {
            Debug.logWarning("No shipment package ID found; cannot update RouteSegment", MODULE);
        }

        return result;
    }


    /**
     * Move a shipment into Packed status and then to Shipped status
     */
    public static Map<String, Object> setShipmentStatusPackedAndShipped(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> packedContext = null;
        packedContext.put("shipmentId", context.get("shipmentId"));
        packedContext.put("statusId", "SHIPMENT_PACKED");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateShipment", packedContext);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateShipment: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(context.get("setPackedOnly"))) {
            packedContext.put("shipmentId", context.get("shipmentId"));
            packedContext.put("statusId", "SHIPMENT_SHIPPED");
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updateShipment", packedContext);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling updateShipment: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        return result;
    }


    /**
     * Quick ships order based on item list
     */
    public static Map<String, Object> quickShipOrderByItem(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue productStore = null;
        Map<String, Object> shipmentContext = null;
        Object issueContext = null;
        GenericValue itemMap = null;
        Map<String, Object> routeSegLookup = null;
        GenericValue packageRouteSegment = null;
        GenericValue itemIssuance = null;
        Map<String, Object> packageMap = null;
        Object shipItemContext = null;
        Map<String, Object> packedContext = null;
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
        if (UtilValidate.isEmpty(context.get("originFacilityId"))) {
            if (UtilValidate.isEmpty(orderHeader.get("productStoreId"))) {
                {
                    String errorMsg = UtilProperties.getMessage("ProductUiLabels", "FacilityNoQuickShip", locale);
                    error_list.add(errorMsg);
                }
                if (!error_list.isEmpty()) {
                    return ServiceUtil.returnError(error_list);
                }
            }
            if (UtilValidate.isNotEmpty(orderHeader.get("productStoreId"))) {
                try {
                    productStore = EntityQuery.use(delegator)
                            .from("ProductStore")
                            .where(UtilMisc.toMap("productStoreId", orderHeader.get("productStoreId")))
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying ProductStore: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (!"Y".equals(productStore.get("reserveInventory"))) {
                    {
                        String errorMsg = UtilProperties.getMessage("ProductUiLabels", "FacilityNoQuickShipForNotReserveInventory", locale);
                        error_list.add(errorMsg);
                    }
                }
                if (!"Y".equals(productStore.get("oneInventoryFacility"))) {
                    {
                        String errorMsg = UtilProperties.getMessage("ProductUiLabels", "FacilityNoQuickShipForMultipleFacilities", locale);
                        error_list.add(errorMsg);
                    }
                }
                if (UtilValidate.isEmpty(productStore.get("inventoryFacilityId"))) {
                    {
                        String errorMsg = UtilProperties.getMessage("ProductUiLabels", "FacilityNoQuickShipForNotInventoryFacility", locale);
                        error_list.add(errorMsg);
                    }
                }
                if (!error_list.isEmpty()) {
                    return ServiceUtil.returnError(error_list);
                }
            }
        }
        if (UtilValidate.isEmpty(context.get("itemShipList"))) {
            {
                String errorMsg = UtilProperties.getMessage("ProductUiLabels", "FacilityNoItemsAvailableToShip", locale);
                error_list.add(errorMsg);
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        }
        Object itemMapList = context.get("itemShipList");
        if (UtilValidate.isNotEmpty(context.get("originFacilityId"))) {
            shipmentContext.put("originFacilityId", context.get("originFacilityId"));
        }
        shipmentContext.put("primaryOrderId", context.get("orderId"));
        shipmentContext.put("primaryShipGroupSeqId", context.get("shipGroupSeqId"));
        shipmentContext.put("statusId", "SHIPMENT_INPUT");
        Map<String, Object> shipmentLookupMap = new HashMap<String, Object>();
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createShipment", shipmentContext);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            shipmentLookupMap.put("shipmentId", serviceResult.get("shipmentId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createShipment: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue shipment = null;
        try {
            shipment = EntityQuery.use(delegator)
                    .from("Shipment")
                    .where(shipmentLookupMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key Shipment: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Debug.logVerbose("ShipMap List : " + itemMapList + "  /  " + context.get("itemShipList"), MODULE);
        if (itemMapList != null) {
            for (Object itemMap_iter : (List<?>) itemMapList) {
                itemMap = (GenericValue) itemMap_iter;
                Debug.logVerbose("Item Map : " + itemMap, MODULE);
                issueContext = new HashMap<String, Object>();
                ((Map<String, Object>) issueContext).put("shipmentId", shipment.get("shipmentId"));
                ((Map<String, Object>) issueContext).put("orderId", context.get("orderId"));
                ((Map<String, Object>) issueContext).put("shipGroupSeqId", context.get("shipGroupSeqId"));
                ((Map<String, Object>) issueContext).put("orderItemSeqId", itemMap.get("orderItemSeqId"));
                ((Map<String, Object>) issueContext).put("inventoryItemId", itemMap.get("inventoryItemId"));
                ((Map<String, Object>) issueContext).put("quantity", itemMap.get("qtyShipped"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("issueOrderItemShipGrpInvResToShipment", (Map<String, Object>) issueContext);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                    itemMap.put("itemIssuanceId", serviceResult.get("itemIssuanceId"));
                } catch (Exception e) {
                    Debug.logError(e, "Error calling issueOrderItemShipGrpInvResToShipment: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }
        if (itemMapList != null) {
            for (Object itemMap_iter : (List<?>) itemMapList) {
                itemMap = (GenericValue) itemMap_iter;
                Debug.logInfo("QuickShipOrderByItem grouping by tracking number : " + itemMap.get("trackingNum"), MODULE);
                try {
                    itemIssuance = EntityQuery.use(delegator)
                            .from("ItemIssuance")
                            .where(UtilMisc.toMap("itemIssuanceId", itemMap.get("itemIssuanceId")))
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying ItemIssuance: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                shipItemContext = new HashMap<String, Object>();
                ((Map<String, Object>) shipItemContext).put("shipmentPackageSeqId", ((Map<String, Object>) ((Map<String, Object>) packageMap).get("${itemMap")).get("trackingNum}"));
                if (UtilValidate.isEmpty(((Map<String, Object>) shipItemContext).get("shipmentPackageSeqId"))) {
                    ((Map<String, Object>) shipItemContext).put("shipmentPackageSeqId", "New");
                }
                Debug.logInfo("Package SeqID : " + ((Map<String, Object>) shipItemContext).get("shipmentPackageSeqId"), MODULE);
                ((Map<String, Object>) shipItemContext).put("shipmentId", itemIssuance.get("shipmentId"));
                ((Map<String, Object>) shipItemContext).put("shipmentItemSeqId", itemIssuance.get("shipmentItemSeqId"));
                ((Map<String, Object>) shipItemContext).put("quantity", itemIssuance.get("quantity"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("addShipmentContentToPackage", (Map<String, Object>) shipItemContext);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                    packageMap.put("${itemMap.trackingNum}", serviceResult.get("shipmentPackageSeqId"));
                    routeSegLookup.put("shipmentPackageSeqId", serviceResult.get("shipmentPackageSeqId"));
                } catch (Exception e) {
                    Debug.logError(e, "Error calling addShipmentContentToPackage: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (UtilValidate.isNotEmpty(((Map<String, Object>) routeSegLookup).get("shipmentPackageSeqId"))) {
                    routeSegLookup.put("shipmentId", itemIssuance.get("shipmentId"));
                    routeSegLookup.put("shipmentRouteSegmentId", "00001");
                    try {
                        packageRouteSegment = EntityQuery.use(delegator)
                                .from("ShipmentPackageRouteSeg")
                                .where(routeSegLookup)
                                .queryOne();
                    } catch (Exception e) {
                        Debug.logError(e, "Error finding by primary key ShipmentPackageRouteSeg: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    if (UtilValidate.isNotEmpty(packageRouteSegment)) {
                        packageRouteSegment.put("trackingCode", itemMap.get("trackingNum"));
                        try {
                            delegator.store(packageRouteSegment);
                        } catch (Exception e) {
                            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                    }
                    if (UtilValidate.isEmpty(packageRouteSegment)) {
                        Debug.logWarning("No route segment found : " + routeSegLookup, MODULE);
                    }
                }
                if (UtilValidate.isEmpty(((Map<String, Object>) routeSegLookup).get("shipmentPackageSeqId"))) {
                    Debug.logWarning("No shipment package ID found; cannot update RouteSegment", MODULE);
                }
            }
        }
        packedContext.put("shipmentId", shipment.get("shipmentId"));
        packedContext.put("statusId", "SHIPMENT_PACKED");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateShipment", packedContext);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateShipment: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(context.get("setPackedOnly"))) {
            packedContext.put("shipmentId", shipment.get("shipmentId"));
            packedContext.put("statusId", "SHIPMENT_SHIPPED");
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updateShipment", packedContext);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling updateShipment: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        result.put("shipmentId", shipment.get("shipmentId"));

        return result;
    }


    /**
     * Delete an OrderShipment and updates the ShipmentItem
     */
    public static Map<String, Object> removeOrderShipmentFromShipment(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> inMap = null;
        GenericValue lookupPk = delegator.makeValue("OrderShipment");
        lookupPk.setPKFields(context);
        GenericValue orderShipment = null;
        try {
            orderShipment = EntityQuery.use(delegator)
                    .from("OrderShipment")
                    .where(lookupPk)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key OrderShipment: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        lookupPk = null;
        lookupPk = delegator.makeValue("ShipmentItem");
        lookupPk.setPKFields(context);
        GenericValue shipmentItem = null;
        try {
            shipmentItem = EntityQuery.use(delegator)
                    .from("ShipmentItem")
                    .where(lookupPk)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ShipmentItem: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        inMap.put("userLogin", context.get("userLogin"));
        inMap.put("shipmentId", context.get("shipmentId"));
        inMap.put("shipmentItemSeqId", context.get("shipmentItemSeqId"));
        inMap.put("orderId", context.get("orderId"));
        inMap.put("orderItemSeqId", context.get("orderItemSeqId"));
        inMap.put("shipGroupSeqId", context.get("shipGroupSeqId"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("deleteOrderShipment", inMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling deleteOrderShipment: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        shipmentItem.set("quantity", new BigDecimal(orderShipment.get("quantity").toString()));
        inMap = new HashMap<String, Object>();
        if (((Comparable) shipmentItem.get("quantity")).compareTo(new BigDecimal("0.0")) > 0) {
            inMap.put("userLogin", context.get("userLogin"));
            inMap.put("shipmentId", context.get("shipmentId"));
            inMap.put("shipmentItemSeqId", context.get("shipmentItemSeqId"));
            inMap.put("quantity", shipmentItem.get("quantity"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updateShipmentItem", inMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling updateShipmentItem: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        } else {
            inMap.put("userLogin", context.get("userLogin"));
            inMap.put("shipmentId", context.get("shipmentId"));
            inMap.put("shipmentItemSeqId", context.get("shipmentItemSeqId"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("deleteShipmentItem", inMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling deleteShipmentItem: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        return result;
    }


    /**
     * Add or update a ShipmentPlan entry
     */
    public static Map<String, Object> addOrderShipmentToShipment(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue orderHeaderLookupPk = null;
        Object remainingQuantity = null;
        GenericValue orderItemLookupPk = null;
        GenericValue orderItem = null;
        List<GenericValue> existingOrderShipments = null;
        GenericValue orderShipmentLookup = null;
        GenericValue orderHeader = null;
        Map<String, Object> inputMap = null;
        if (((Comparable) context.get("quantity")).compareTo(BigDecimal.ZERO) > 0) {
            orderHeaderLookupPk = delegator.makeValue("OrderHeader");
            orderHeaderLookupPk.setPKFields(context);
            try {
                orderHeader = EntityQuery.use(delegator)
                        .from(orderHeaderLookupPk.getEntityName())
                        .where(orderHeaderLookupPk)
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error finding by primary key : " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            orderItemLookupPk = delegator.makeValue("OrderItem");
            orderItemLookupPk.setPKFields(context);
            try {
                orderItem = EntityQuery.use(delegator)
                        .from(orderItemLookupPk.getEntityName())
                        .where(orderItemLookupPk)
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error finding by primary key : " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            orderShipmentLookup = delegator.makeValue("OrderShipment");
            orderShipmentLookup.setPKFields(context);
            try {
                existingOrderShipments = EntityQuery.use(delegator)
                        .from("OrderShipment")
                        .where(orderShipmentLookup)
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying OrderShipment: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (UtilValidate.isNotEmpty(existingOrderShipments)) {
                error_list.add("Not adding Order Item to plan for shipment [" + context.get("shipmentId") + "] because the order item is already in the shipment (order [" + context.get("orderId") + "], order item [" + context.get("orderItemSeqId") + "])");
            }
            inputMap.put("orderId", context.get("orderId"));
            inputMap.put("orderItemSeqId", context.get("orderItemSeqId"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("getQuantityForShipment", inputMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
                remainingQuantity = serviceResult.get("remainingQuantity");
            } catch (Exception e) {
                Debug.logError(e, "Error calling getQuantityForShipment: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (context.get("quantity") != null /* TODO: field compare operator greater */) {
                error_list.add("Not adding Order Item to plan for shipment [" + context.get("shipmentId") + "] because the quantity is greater than the remaining quantity (order [" + context.get("orderId") + "], order item [" + context.get("orderItemSeqId") + "])");
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
            inputMap = new HashMap<String, Object>();
            inputMap.put("userLogin", context.get("userLogin"));
            inputMap.put("shipmentId", context.get("shipmentId"));
            inputMap.put("productId", orderItem.get("productId"));
            inputMap.put("quantity", context.get("quantity"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createShipmentItem", inputMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
                context.put("shipmentItemSeqId", serviceResult.get("shipmentItemSeqId"));
                result.put("shipmentItemSeqId", serviceResult.get("shipmentItemSeqId"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling createShipmentItem: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            inputMap = new HashMap<String, Object>();
            inputMap.put("userLogin", context.get("userLogin"));
            inputMap.put("shipmentId", context.get("shipmentId"));
            inputMap.put("shipmentItemSeqId", context.get("shipmentItemSeqId"));
            inputMap.put("orderId", context.get("orderId"));
            inputMap.put("orderItemSeqId", context.get("orderItemSeqId"));
            inputMap.put("quantity", context.get("quantity"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createOrderShipment", context);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling createOrderShipment: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        return result;
    }


    /**
     * get the order item quantity still not put in shipments
     */
    public static Map<String, Object> getQuantityForShipment(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Object plannedQuantity = null;
        Object issuedQuantity = null;
        GenericValue orderItemLookupPk = delegator.makeValue("OrderItem");
        orderItemLookupPk.setPKFields(context);
        GenericValue orderItem = null;
        try {
            orderItem = EntityQuery.use(delegator)
                    .from(orderItemLookupPk.getEntityName())
                    .where(orderItemLookupPk)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key : " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> orderShipmentLookup = new HashMap<String, Object>();
        orderShipmentLookup.put("orderId", context.get("orderId"));
        orderShipmentLookup.put("orderItemSeqId", context.get("orderItemSeqId"));
        List<GenericValue> existingOrderShipments = null;
        try {
            existingOrderShipments = EntityQuery.use(delegator)
                    .from("OrderShipment")
                    .where(orderShipmentLookup)
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderShipment: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (existingOrderShipments != null) {
            for (GenericValue orderShipment : existingOrderShipments) {
                plannedQuantity = new BigDecimal(orderShipment.get("quantity").toString());
            }
        }
        existingOrderShipments = null;
        try {
            existingOrderShipments = EntityQuery.use(delegator)
                    .from("ItemIssuance")
                    .where(orderShipmentLookup)
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ItemIssuance: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (existingOrderShipments != null) {
            for (GenericValue itemIssuance : existingOrderShipments) {
                issuedQuantity = new BigDecimal(issuedQuantity.toString());
            }
        }
        Object totPlannedOrIssuedQuantity = new BigDecimal(issuedQuantity.toString());
        Object remainingQuantity = (new BigDecimal(orderItem.get("cancelQuantity").toString())).subtract(new BigDecimal(totPlannedOrIssuedQuantity.toString()));
        result.put("remainingQuantity", remainingQuantity);

        return result;
    }


    /**
     * Check Shipment Items and Cancel Item Issuance and Order Shipment
     */
    public static Map<String, Object> checkCancelItemIssuanceAndOrderShipmentFromShipment(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> deleteOrderShipmentMap = null;
        Map<String, Object> inputMap = null;
        List<GenericValue> orderShipmentList = null;
        try {
            orderShipmentList = EntityQuery.use(delegator)
                    .from("OrderShipment")
                    .where(UtilMisc.toMap("shipmentId", context.get("shipmentId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (orderShipmentList != null) {
            for (GenericValue orderShipment : orderShipmentList) {
                deleteOrderShipmentMap = new HashMap<String, Object>();
                // set-service-fields from "orderShipment" to "deleteOrderShipmentMap" for service "deleteOrderShipment"
                deleteOrderShipmentMap.putAll(UtilMisc.toMap(orderShipment));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("deleteOrderShipment", deleteOrderShipmentMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling deleteOrderShipment: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }
        Debug.logInfo("Cancelling Item Issuances for shimpentId: " + context.get("shipmentId"), MODULE);
        GenericValue shipment = null;
        try {
            shipment = EntityQuery.use(delegator)
                    .from("Shipment")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Shipment: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        List<GenericValue> issuances = null;
        try {
            issuances = shipment.getRelated("ItemIssuance", null, null, false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related ItemIssuance: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (issuances != null) {
            for (GenericValue issuance : issuances) {
                inputMap.put("itemIssuanceId", issuance.get("itemIssuanceId"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("cancelOrderItemIssuanceFromSalesShipment", inputMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling cancelOrderItemIssuanceFromSalesShipment: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }

        return result;
    }


    /**
     * Create a QuoteAttribute
     */
    public static Map<String, Object> createQuantityBreak(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue quantityBreak = delegator.makeValue("QuantityBreak");
        quantityBreak.setNonPKFields(context);
        ((GenericValue) quantityBreak).put("quantityBreakId", delegator.getNextSeqId("QuantityBreak"));
        try {
            delegator.create(quantityBreak);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }

        return result;
    }


    /**
     * Update an existing QuantityBreak
     */
    public static Map<String, Object> updateQuantityBreak(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue quantityBreak = null;
        try {
            quantityBreak = EntityQuery.use(delegator)
                    .from("QuantityBreak")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying QuantityBreak: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        quantityBreak.setNonPKFields(context);
        try {
            delegator.store(quantityBreak);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }

        return result;
    }


    /**
     * Remove an existing QuantityBreak
     */
    public static Map<String, Object> deleteQuantityBreak(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue quantityBreak = null;
        try {
            quantityBreak = EntityQuery.use(delegator)
                    .from("QuantityBreak")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying QuantityBreak: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        try {
            delegator.removeValue(quantityBreak);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }

        return result;
    }

}

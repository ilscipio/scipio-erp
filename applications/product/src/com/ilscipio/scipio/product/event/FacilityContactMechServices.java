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

import java.sql.Timestamp;
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
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://product/script/org/ofbiz/product/storage/FacilityContactMechServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class FacilityContactMechServices {

    private static final String MODULE = FacilityContactMechServices.class.getName();


    /**
     * Create a FacilityContactMech
     */
    public static Map<String, Object> createFacilityContactMech(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> createContactMechMap = null;
        GenericValue newValue = null;
        GenericValue facilityContactMechPurpose = null;
        newValue = delegator.makeValue("FacilityContactMech");
        GenericValue newFacilityContactMech = delegator.makeValue("FacilityContactMech");
        Debug.logInfo("contactMechId is " + context.get("contactMechId"), MODULE);
        if (UtilValidate.isEmpty(context.get("contactMechId"))) {
            // set-service-fields from "parameters" to "createContactMechMap" for service "createContactMech"
            createContactMechMap.putAll(UtilMisc.toMap(context));
            createContactMechMap.put("contactMechTypeId", context.get("contactMechTypeId"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createContactMech", createContactMechMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
                newValue.put("contactMechId", serviceResult.get("contactMechId"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling createContactMech: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            Debug.logInfo("ContactMech created", MODULE);
        } else {
            newValue.put("contactMechId", context.get("contactMechId"));
        }
        Debug.logInfo("Creating a FacilityContactMech with id: " + context.get("contactMechId"), MODULE);
        newValue.put("facilityId", context.get("facilityId"));
        result.put("contactMechId", newValue.get("contactMechId"));
        result.put("contactMechId", newValue.get("contactMechId"));
        newValue.setNonPKFields(context);
        Timestamp newValue_fromDate = new Timestamp(System.currentTimeMillis());
        try {
            delegator.create(newValue);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(context.get("contactMechPurposeTypeId"))) {
            facilityContactMechPurpose = delegator.makeValue("FacilityContactMechPurpose");
            facilityContactMechPurpose.setPKFields((Map<String, Object>) newValue);
            facilityContactMechPurpose.put("contactMechPurposeTypeId", context.get("contactMechPurposeTypeId"));
            try {
                delegator.create(facilityContactMechPurpose);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        return result;
    }


    /**
     * Lookup a FacilityContactMech (logic from updateFacilityContactMech)
     */
    public static Map<String, Object> lookupFacilityContactMech(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        List<GenericValue> facilityContactMechs = null;
        try {
            facilityContactMechs = EntityQuery.use(delegator)
                    .from("FacilityContactMech")
                    .where(UtilMisc.toMap("facilityId", context.get("facilityId"), "contactMechId", context.get("contactMechId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(facilityContactMechs)) {
            {
                String errorMsg = UtilProperties.getMessage("PartyUiLabels", "PartyCannotUpdateContactBecauseNotWithSpecifiedParty", locale);
                error_list.add(errorMsg);
            }
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        List<GenericValue> validFacilityContactMechs = EntityUtil.filterByDate(UtilGenerics.cast(facilityContactMechs));
        GenericValue facilityContactMech = EntityUtil.getFirst((List<GenericValue>) validFacilityContactMechs);
        if (UtilValidate.isEmpty(facilityContactMech)) {
            {
                String errorMsg = UtilProperties.getMessage("PartyErrorUiLabels", "contactmechservices.cannot_update_specified_contact_info_expired", locale);
                error_list.add(errorMsg);
            }
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }

        return result;
    }


    /**
     * Update a FacilityContactMech
     */
    public static Map<String, Object> updateFacilityContactMech(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue facilityContactMech = null;
        Map<String, Object> inlineResult = null;
        GenericValue newFacilityContactMech = null;
        Map<String, Object> updateContactMechMap = null;
        GenericValue facilityContactMechPurpose = null;
        List<GenericValue> facilityContactMechPurposes = null;
        List<GenericValue> emptyField = null;
        Map<String, Object> purposeMap = null;
        List<GenericValue> purposeResult = null;
        List<GenericValue> facilityContactMechs = null;
        List<GenericValue> validFacilityContactMechs = null;
        if (UtilValidate.isNotEmpty(context.get("facilityContactMech"))) {
            facilityContactMech = (GenericValue) context.get("facilityContactMech");
        } else {
            inlineResult = lookupFacilityContactMech(dctx, context);
            if (ServiceUtil.isError(inlineResult)) {
                return inlineResult;
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        }
        newFacilityContactMech = delegator.makeValue("FacilityContactMech");
        newFacilityContactMech = GenericValue.create((GenericValue) facilityContactMech);
        if (UtilValidate.isEmpty(context.get("newContactMechId"))) {
            // set-service-fields from "parameters" to "updateContactMechMap" for service "updateContactMech"
            updateContactMechMap.putAll(UtilMisc.toMap(context));
            updateContactMechMap.put("contactMechTypeId", context.get("contactMechTypeId"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updateContactMech", updateContactMechMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
                newFacilityContactMech.put("contactMechId", serviceResult.get("contactMechId"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling updateContactMech: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        } else {
            newFacilityContactMech.put("contactMechId", context.get("newContactMechId"));
            Debug.logInfo("Using supplied new contact mech id: " + newFacilityContactMech.get("contactMechId"), MODULE);
        }
        GenericValue facilityContactMechPurposeOld = null;
        if (!java.util.Objects.equals(context.get("contactMechId"), newFacilityContactMech.get("contactMechId"))) {
            newFacilityContactMech.setNonPKFields(context);
            Timestamp newFacilityContactMech_fromDate = new Timestamp(System.currentTimeMillis());
            Timestamp facilityContactMech_thruDate = new Timestamp(System.currentTimeMillis());
            try {
                delegator.store(facilityContactMech);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            try {
                delegator.create(newFacilityContactMech);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            try {
                facilityContactMechPurposes = facilityContactMech.getRelated("FacilityContactMechPurpose", null, null, false);
            } catch (Exception e) {
                Debug.logError(e, "Error getting related FacilityContactMechPurpose: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            emptyField = EntityUtil.filterByDate(UtilGenerics.cast(facilityContactMechPurposes));
            if (facilityContactMechPurposes != null) {
                for (GenericValue facilityContactMechPurposeOldEntry : facilityContactMechPurposes) {
                    facilityContactMechPurpose = GenericValue.create((GenericValue) facilityContactMechPurposeOldEntry);
                    Timestamp facilityContactMechPurposeOld_thruDate = new Timestamp(System.currentTimeMillis());
                    try {
                        delegator.store(facilityContactMechPurposeOldEntry);
                    } catch (Exception e) {
                        Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    facilityContactMechPurpose.put("contactMechId", newFacilityContactMech.get("contactMechId"));
                    purposeMap.put("facilityId", facilityContactMechPurpose.get("facilityId"));
                    purposeMap.put("contactMechPurposeTypeId", facilityContactMechPurpose.get("contactMechPurposeTypeId"));
                    purposeMap.put("contactMechId", facilityContactMechPurpose.get("contactMechId"));
                    try {
                        purposeResult = EntityQuery.use(delegator)
                                .from("FacilityContactMechPurpose")
                                .where(purposeMap)
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying FacilityContactMechPurpose: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    if (UtilValidate.isEmpty(purposeResult)) {
                        try {
                            delegator.create(facilityContactMechPurpose);
                        } catch (Exception e) {
                            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                    }
                }
            }
            Debug.logInfo("Setting id to result: " + newFacilityContactMech.get("contactMechId"), MODULE);
            result.put("contactMechId", newFacilityContactMech.get("contactMechId"));
            result.put("contactMechId", newFacilityContactMech.get("contactMechId"));
        } else {
            facilityContactMech.setNonPKFields(context);
            try {
                delegator.store(facilityContactMech);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            Debug.logInfo("Setting id to result: " + facilityContactMech.get("contactMechId"), MODULE);
            result.put("contactMechId", facilityContactMech.get("contactMechId"));
            result.put("contactMechId", facilityContactMech.get("contactMechId"));
        }

        return result;
    }


    /**
     * Delete a FacilityContactMech Only
     */
    public static Map<String, Object> deleteFacilityContactMechOnly(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue facilityContactMech = null;
        GenericValue newFacilityContactMech = delegator.makeValue("FacilityContactMech");
        GenericValue facilityContactMechMap = delegator.makeValue("FacilityContactMech");
        facilityContactMechMap.setPKFields(context);
        List<GenericValue> facilityContactMechs = null;
        try {
            facilityContactMechs = EntityQuery.use(delegator)
                    .from("FacilityContactMech")
                    .where(facilityContactMechMap)
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying FacilityContactMech: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        List<GenericValue> validFacilityContactMechs = EntityUtil.filterByDate(UtilGenerics.cast(facilityContactMechs));
        facilityContactMech = EntityUtil.getFirst((List<GenericValue>) validFacilityContactMechs);
        if (UtilValidate.isEmpty(facilityContactMech)) {
            {
                String errorMsg = UtilProperties.getMessage("PartyUiLabels", "PartyContactMechNotFoundCannotDelete", locale);
                error_list.add(errorMsg);
            }
            return result;
        }
        if (UtilValidate.isNotEmpty(context.get("nowTimestamp"))) {
            facilityContactMech.put("thruDate", context.get("nowTimestamp"));
        } else {
            Timestamp facilityContactMech_thruDate = new Timestamp(System.currentTimeMillis());
        }
        try {
            delegator.store(facilityContactMech);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Delete a FacilityContactMech And Purposes
     */
    public static Map<String, Object> deleteFacilityContactMechAndPurposes(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        List<GenericValue> facilityContactMechs = null;
        GenericValue facilityContactMech = null;
        GenericValue newFacilityContactMech = null;
        List<GenericValue> validFacilityContactMechs = null;
        GenericValue facilityContactMechMap = null;
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        Map<String, Object> inlineResult = deleteFacilityContactMechOnly(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        List<GenericValue> facilityContactMechPurposes = null;
        try {
            facilityContactMechPurposes = facilityContactMech.getRelated("FacilityContactMechPurpose", null, null, false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related FacilityContactMechPurpose: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        List<GenericValue> emptyField = EntityUtil.filterByDate(UtilGenerics.cast(facilityContactMechPurposes), (Timestamp) nowTimestamp);
        if (facilityContactMechPurposes != null) {
            for (GenericValue facilityContactMechPurposeOld : facilityContactMechPurposes) {
                facilityContactMechPurposeOld.put("thruDate", nowTimestamp);
                try {
                    delegator.store(facilityContactMechPurposeOld);
                } catch (Exception e) {
                    Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }

        return result;
    }


    /**
     * Delete a FacilityContactMech And Purposes
     */
    public static Map<String, Object> deleteFacilityContactMech(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        List<GenericValue> facilityContactMechPurposes = null;
        List<GenericValue> emptyField = null;
        GenericValue facilityContactMechPurposeOld = null;
        Timestamp nowTimestamp = null;
        Map<String, Object> inlineResult = null;
        inlineResult = deleteFacilityContactMechAndPurposes(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }

        return result;
    }


    /**
     * Create a PostalAddress for facility
     */
    public static Map<String, Object> createFacilityPostalAddress(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Map<String, Object> createPostalAddressMap = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "createPostalAddressMap" for service "createPostalAddress"
        createPostalAddressMap.putAll(UtilMisc.toMap(context));
        Map<String, Object> newFacilityContactMech = new HashMap<String, Object>();
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createPostalAddress", createPostalAddressMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            newFacilityContactMech.put("contactMechId", serviceResult.get("contactMechId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createPostalAddress: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        Map<String, Object> createFacilityContactMechMap = new HashMap<String, Object>();
        createFacilityContactMechMap.put("contactMechId", ((Map<String, Object>) newFacilityContactMech).get("contactMechId"));
        // set-service-fields from "parameters" to "createFacilityContactMechMap" for service "createFacilityContactMech"
        createFacilityContactMechMap.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createFacilityContactMech", createFacilityContactMechMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createFacilityContactMech: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        result.put("contactMechId", ((Map<String, Object>) newFacilityContactMech).get("contactMechId"));
        result.put("contactMechId", ((Map<String, Object>) newFacilityContactMech).get("contactMechId"));

        return result;
    }


    /**
     * Update a PostalAddress for facility
     */
    public static Map<String, Object> updateFacilityPostalAddress(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue newFacilityContactMech = delegator.makeValue("FacilityContactMech");
        Map<String, Object> updatePostalAddressMap = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "updatePostalAddressMap" for service "updatePostalAddress"
        updatePostalAddressMap.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updatePostalAddress", updatePostalAddressMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            newFacilityContactMech.put("contactMechId", serviceResult.get("contactMechId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling updatePostalAddress: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> updateFacilityContactMechMap = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "updateFacilityContactMechMap" for service "updateFacilityContactMech"
        updateFacilityContactMechMap.putAll(UtilMisc.toMap(context));
        updateFacilityContactMechMap.put("newContactMechId", newFacilityContactMech.get("contactMechId"));
        updateFacilityContactMechMap.put("contactMechTypeId", "POSTAL_ADDRESS");
        Debug.logInfo("Copied id to updateFacilityContactMechMap: " + ((Map<String, Object>) updateFacilityContactMechMap).get("newContactMechId"), MODULE);
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateFacilityContactMech", updateFacilityContactMechMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateFacilityContactMech: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        result.put("contactMechId", newFacilityContactMech.get("contactMechId"));
        result.put("contactMechId", newFacilityContactMech.get("contactMechId"));

        return result;
    }


    /**
     * Create a TelecomNumber for facility
     */
    public static Map<String, Object> createFacilityTelecomNumber(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Debug.logInfo("Creating telecom number", MODULE);
        Map<String, Object> createTelecomNumberMap = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "createTelecomNumberMap" for service "createTelecomNumber"
        createTelecomNumberMap.putAll(UtilMisc.toMap(context));
        Map<String, Object> newFacilityContactMech = new HashMap<String, Object>();
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createTelecomNumber", createTelecomNumberMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            newFacilityContactMech.put("contactMechId", serviceResult.get("contactMechId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createTelecomNumber: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> createFacilityContactMechMap = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "createFacilityContactMechMap" for service "createFacilityContactMech"
        createFacilityContactMechMap.putAll(UtilMisc.toMap(context));
        createFacilityContactMechMap.put("contactMechId", ((Map<String, Object>) newFacilityContactMech).get("contactMechId"));
        Debug.logInfo("Copied id to createFacilityContactMechMap: " + ((Map<String, Object>) createFacilityContactMechMap).get("contactMechId"), MODULE);
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createFacilityContactMech", createFacilityContactMechMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createFacilityContactMech: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        result.put("contactMechId", ((Map<String, Object>) newFacilityContactMech).get("contactMechId"));
        result.put("contactMechId", ((Map<String, Object>) newFacilityContactMech).get("contactMechId"));

        return result;
    }


    /**
     * Update a TelecomNumber for facility
     */
    public static Map<String, Object> updateFacilityTelecomNumber(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Map<String, Object> updateTelecomNumberMap = null;
        List<GenericValue> facilityContactMechs = null;
        GenericValue facilityContactMech = null;
        List<GenericValue> validFacilityContactMechs = null;
        Map<String, Object> inlineResult = lookupFacilityContactMech(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        Map<String, Object> updateFacilityContactMechMap = new HashMap<String, Object>();
        updateFacilityContactMechMap.put("facilityContactMech", facilityContactMech);
        Object extensionPresent = (Boolean) GroovyUtil.eval("parameters.containsKey('extension')", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
        if (Boolean.TRUE.equals(extensionPresent)) {
            if (!java.util.Objects.equals(context.get("extension"), facilityContactMech.get("extension"))) {
                updateTelecomNumberMap.put("forceNewRecord", Boolean.TRUE);
            }
        }
        GenericValue newFacilityContactMech = delegator.makeValue("FacilityContactMech");
        // set-service-fields from "parameters" to "updateTelecomNumberMap" for service "updateTelecomNumber"
        updateTelecomNumberMap.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateTelecomNumber", updateTelecomNumberMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            newFacilityContactMech.put("contactMechId", serviceResult.get("contactMechId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateTelecomNumber: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        // set-service-fields from "parameters" to "updateFacilityContactMechMap" for service "updateFacilityContactMechGiven"
        updateFacilityContactMechMap.putAll(UtilMisc.toMap(context));
        updateFacilityContactMechMap.put("newContactMechId", newFacilityContactMech.get("contactMechId"));
        updateFacilityContactMechMap.put("contactMechTypeId", "TELECOM_NUMBER");
        Debug.logInfo("Copied id to updateFacilityContactMechMap: " + ((Map<String, Object>) updateFacilityContactMechMap).get("newContactMechId"), MODULE);
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateFacilityContactMechGiven", updateFacilityContactMechMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateFacilityContactMechGiven: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Debug.logInfo("Setting result id: " + newFacilityContactMech.get("contactMechId"), MODULE);
        result.put("contactMechId", newFacilityContactMech.get("contactMechId"));
        result.put("contactMechId", newFacilityContactMech.get("contactMechId"));

        return result;
    }


    /**
     * Create an email address for facility
     */
    public static Map<String, Object> createFacilityEmailAddress(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        if (UtilValidate.isEmail((String) context.get("emailAddress"))) {
        } else {
            {
                String errorMsg = UtilProperties.getMessage("PartyUiLabels", "PartyEmailAddressNotFormattedCorrectly", locale);
                error_list.add(errorMsg);
            }
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        Map<String, Object> createFacilityContactMechMap = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "createFacilityContactMechMap" for service "createFacilityContactMech"
        createFacilityContactMechMap.putAll(UtilMisc.toMap(context));
        createFacilityContactMechMap.put("infoString", context.get("emailAddress"));
        createFacilityContactMechMap.put("contactMechTypeId", "EMAIL_ADDRESS");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createFacilityContactMech", createFacilityContactMechMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            result.put("contactMechId", serviceResult.get("contactMechId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createFacilityContactMech: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Update an email address for facility
     */
    public static Map<String, Object> updateFacilityEmailAddress(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        if (UtilValidate.isEmail((String) context.get("emailAddress"))) {
        } else {
            {
                String errorMsg = UtilProperties.getMessage("PartyUiLabels", "PartyEmailAddressNotFormattedCorrectly", locale);
                error_list.add(errorMsg);
            }
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        Map<String, Object> updateFacilityContactMechMap = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "updateFacilityContactMechMap" for service "updateFacilityContactMech"
        updateFacilityContactMechMap.putAll(UtilMisc.toMap(context));
        updateFacilityContactMechMap.put("infoString", context.get("emailAddress"));
        updateFacilityContactMechMap.put("contactMechTypeId", "EMAIL_ADDRESS");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateFacilityContactMech", updateFacilityContactMechMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            result.put("contactMechId", serviceResult.get("contactMechId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateFacilityContactMech: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Create a facility/contact mech purpose
     */
    public static Map<String, Object> createFacilityContactMechPurpose(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue lookUpMap = delegator.makeValue("FacilityContactMechPurpose");
        lookUpMap.put("facilityId", context.get("facilityId"));
        lookUpMap.put("contactMechId", context.get("contactMechId"));
        lookUpMap.put("contactMechPurposeTypeId", context.get("contactMechPurposeTypeId"));
        List<GenericValue> purposeList = null;
        try {
            purposeList = EntityQuery.use(delegator)
                    .from("FacilityContactMechPurpose")
                    .where(lookUpMap)
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying FacilityContactMechPurpose: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        List<GenericValue> emptyField = EntityUtil.filterByDate(UtilGenerics.cast(purposeList));
        if (UtilValidate.isNotEmpty(purposeList)) {
            {
                String errorMsg = UtilProperties.getMessage("PartyUiLabels", "PartyCouldNotCreateNewPurpose", locale);
                error_list.add(errorMsg);
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        }
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        GenericValue newEntity = delegator.makeValue("FacilityContactMechPurpose");
        newEntity.put("facilityId", context.get("facilityId"));
        newEntity.put("contactMechId", context.get("contactMechId"));
        newEntity.put("contactMechPurposeTypeId", context.get("contactMechPurposeTypeId"));
        newEntity.put("fromDate", nowTimestamp);
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        result.put("fromDate", newEntity.get("fromDate"));

        return result;
    }


    /**
     * Delete a facility/contact mech purpose
     */
    public static Map<String, Object> deleteFacilityContactMechPurpose(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue lookUpMap = delegator.makeValue("FacilityContactMechPurpose");
        lookUpMap.setPKFields(context);
        GenericValue purposeEntity = null;
        try {
            purposeEntity = EntityQuery.use(delegator)
                    .from("FacilityContactMechPurpose")
                    .where(lookUpMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key FacilityContactMechPurpose: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(purposeEntity)) {
            {
                String errorMsg = UtilProperties.getMessage("PartyUiLabels", "PartyUnableToLocatePurpose", locale);
                error_list.add(errorMsg);
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        }
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        purposeEntity.put("thruDate", nowTimestamp);
        try {
            delegator.store(purposeEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }

}

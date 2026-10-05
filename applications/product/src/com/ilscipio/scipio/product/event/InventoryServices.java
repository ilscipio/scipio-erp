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
import org.ofbiz.security.Security;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://product/script/org/ofbiz/product/inventory/InventoryServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class InventoryServices {

    private static final String MODULE = InventoryServices.class.getName();


    /**
     * Check Facility Related Permission
     */
    public static Map<String, Object> checkFacilityRelatedPermission(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Security security = dctx.getSecurity();
        List<String> error_list = new LinkedList<>();

        Object alternatePermissionRoot = context.get("alternatePermissionRoot");
        String callingMethodName = null;
        Object checkAction = null;
        if (UtilValidate.isEmpty(callingMethodName)) {
            callingMethodName = UtilProperties.getMessage("CommonUiLabels", "CommonPermissionThisOperation", locale);
        }
        if (UtilValidate.isEmpty(checkAction)) {
            checkAction = "UPDATE";
        }
        if (!((security.hasEntityPermission("CATALOG", "_${checkAction}", userLogin) || security.hasPermission("CATALOG_ADMIN", userLogin) || security.hasEntityPermission("FACILITY", "_${checkAction}", userLogin) || security.hasPermission("FACILITY_ADMIN", userLogin) || (!(UtilValidate.isEmpty(alternatePermissionRoot)) && security.hasEntityPermission("${alternatePermissionRoot}", "_${checkAction}", userLogin))))) {
            {
                String errorMsg = UtilProperties.getMessage("ProductUiLabels", "ProductCatalogCreatePermissionError", locale);
                error_list.add(errorMsg);
            }
        }

        return result;
    }


    /**
     * Main permission logic
     */
    public static Map<String, Object> facilityGenericPermission(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Boolean hasPermission = null;
        String failMessage = null;
        Object checkAction = null;
        String callingMethodName = null;
        Object mainAction = context.get("mainAction");
        if (UtilValidate.isEmpty(mainAction)) {
            {
                String errorMsg = UtilProperties.getMessage("ProductUiLabels", "ProductMissingMainActionInPermissionService", locale);
                error_list.add(errorMsg);
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        }
        callingMethodName = (String) context.get("resourceDescription");
        checkAction = context.get("mainAction");
        Map<String, Object> inlineResult = checkFacilityRelatedPermission(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (UtilValidate.isEmpty(error_list)) {
            hasPermission = Boolean.TRUE;
            result.put("hasPermission", hasPermission);
        } else {
            failMessage = UtilProperties.getMessage("ProductUiLabels", "ProductFacilityPermissionError", locale);
            hasPermission = Boolean.FALSE;
            result.put("hasPermission", hasPermission);
            result.put("failMessage", failMessage);
        }

        return result;
    }


    /**
     * ProductFacility Permission Checking Logic
     */
    public static Map<String, Object> checkProductFacilityRelatedPermission(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Object mainAction = null;
        String resourceDescription = null;
        Boolean hasPermission = null;
        String failMessage = null;
        if (UtilValidate.isEmpty(mainAction)) {
            mainAction = context.get("mainAction");
            if (UtilValidate.isEmpty(mainAction)) {
                {
                    String errorMsg = UtilProperties.getMessage("CommonUiLabels", "CommonPermissionMainActionAttributeMissing", locale);
                    error_list.add(errorMsg);
                }
            }
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        resourceDescription = (String) context.get("resourceDescription");
        if (UtilValidate.isEmpty(resourceDescription)) {
            resourceDescription = UtilProperties.getMessage("CommonUiLabels", "CommonPermissionThisOperation", locale);
        }
        Object callingMethodName = resourceDescription;
        Object checkAction = mainAction;
        Object alternatePermissionRoot = "FACILITY";
        // TODO: Call simple-method "checkProductRelatedPermission" from "component://product/script/org/ofbiz/product/product/ProductServices.xml"
        if (UtilValidate.isEmpty(error_list)) {
            hasPermission = Boolean.TRUE;
            result.put("hasPermission", hasPermission);
        } else {
            failMessage = UtilProperties.getMessage("ProductUiLabels", "ProductFacilityPermissionError", locale);
            hasPermission = Boolean.FALSE;
            result.put("hasPermission", hasPermission);
            result.put("failMessage", failMessage);
        }

        return result;
    }


    /**
     * Create an InventoryItem
     */
    public static Map<String, Object> createInventoryItem(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue lot = null;
        List<GenericValue> lotList = null;
        GenericValue inventoryItem = null;
        Object accPref = null;
        Map<String, Object> partyAccountingPreferencesCallMap = null;
        Object updateInventoryItem = null;
        Map<String, Object> inputMap = null;
        GenericValue facility = null;
        GenericValue product = null;
        try {
            product = EntityQuery.use(delegator)
                    .from("Product")
                    .where(UtilMisc.toMap("productId", context.get("productId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Product: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (("Mandatory".equals(product.get("lotIdFilledIn")) && UtilValidate.isEmpty(context.get("lotId")))) {
            {
                String errorMsg = UtilProperties.getMessage("ProductErrorUiLabels", "ProductLotIdMandatory", locale);
                error_list.add(errorMsg);
            }
        }
        if (("Forbidden".equals(product.get("lotIdFilledIn")) && !(UtilValidate.isEmpty(context.get("lotId"))))) {
            {
                String errorMsg = UtilProperties.getMessage("ProductErrorUiLabels", "ProductLotIdForbidden", locale);
                error_list.add(errorMsg);
            }
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        if ("N".equals(context.get("isReturned"))) {
            if (UtilValidate.isNotEmpty(context.get("lotId"))) {
                try {
                    lotList = EntityQuery.use(delegator)
                            .from("Lot")
                            .where(UtilMisc.toMap("lotId", context.get("lotId")))
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (UtilValidate.isEmpty(lotList)) {
                    lot = delegator.makeValue("Lot");
                    lot.put("lotId", context.get("lotId"));
                    try {
                        delegator.create(lot);
                    } catch (Exception e) {
                        Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            }
        }
        inventoryItem = delegator.makeValue("InventoryItem");
        inventoryItem.setNonPKFields(context);
        Map<String, Object> inlineResult = inventoryItemCheckSetDefaultValues(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        ((GenericValue) inventoryItem).put("inventoryItemId", delegator.getNextSeqId("InventoryItem"));
        try {
            delegator.create(inventoryItem);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        result.put("inventoryItemId", inventoryItem.get("inventoryItemId"));

        return result;
    }


    /**
     * createInventoryItemCheckSetAtpQoh
     */
    public static Map<String, Object> createInventoryItemCheckSetAtpQoh(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> createDetailMap = null;
        Object createDetailMap_inventoryItemId = null;
        Object createDetailMap_availableToPromiseDiff = null;
        Object createDetailMap_quantityOnHandDiff = null;
        if ((!(UtilValidate.isEmpty(context.get("availableToPromiseTotal"))) || !(UtilValidate.isEmpty(context.get("quantityOnHandTotal"))))) {
            Debug.logInfo("Got an InventoryItem with ATP/QOH Total with ID " + context.get("inventoryItemId") + ", creating InventoryItemDetail", MODULE);
            createDetailMap.put("inventoryItemId", context.get("inventoryItemId"));
            createDetailMap.put("availableToPromiseDiff", context.get("availableToPromiseTotal"));
            createDetailMap.put("quantityOnHandDiff", context.get("quantityOnHandTotal"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createInventoryItemDetail", createDetailMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling createInventoryItemDetail: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        return result;
    }


    /**
     * Check and, if empty, fills with default values ownerPartyId, currencyUomId, unitCost
     */
    public static Map<String, Object> inventoryItemCheckSetDefaultValues(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue inventoryItem = null;
        Object updateInventoryItem = null;
        GenericValue facility = null;
        Object accPref = null;
        Map<String, Object> partyAccountingPreferencesCallMap = null;
        Map<String, Object> inputMap = null;
        if (UtilValidate.isEmpty(inventoryItem)) {
            try {
                inventoryItem = EntityQuery.use(delegator)
                        .from("InventoryItem")
                        .where(context)
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying InventoryItem: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            updateInventoryItem = "Y";
        }
        if ((!(UtilValidate.isEmpty(inventoryItem.get("facilityId"))) && !(UtilValidate.isEmpty(inventoryItem.get("ownerPartyId"))) && !(UtilValidate.isEmpty(inventoryItem.get("currencyUomId"))) && !(UtilValidate.isEmpty(inventoryItem.get("unitCost"))))) {
            return result;
        }
        if (UtilValidate.isEmpty(inventoryItem.get("facilityId"))) {
            {
                String errorMsg = UtilProperties.getMessage("ProductUiLabels", "FacilityInventoryItemsMissingFacilityId", locale);
                error_list.add(errorMsg);
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        }
        if (UtilValidate.isEmpty(inventoryItem.get("ownerPartyId"))) {
            try {
                facility = inventoryItem.getRelatedOne("Facility", false);
            } catch (Exception e) {
                Debug.logError(e, "Error getting related one Facility: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            inventoryItem.put("ownerPartyId", facility.get("ownerPartyId"));
            if (UtilValidate.isEmpty(inventoryItem.get("ownerPartyId"))) {
                {
                    String errorMsg = UtilProperties.getMessage("ProductUiLabels", "FacilityInventoryItemsMissingOwnerPartyId", locale);
                    error_list.add(errorMsg);
                }
                if (!error_list.isEmpty()) {
                    return ServiceUtil.returnError(error_list);
                }
            }
        }
        if (UtilValidate.isEmpty(inventoryItem.get("currencyUomId"))) {
            partyAccountingPreferencesCallMap.put("organizationPartyId", inventoryItem.get("ownerPartyId"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("getPartyAccountingPreferences", partyAccountingPreferencesCallMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
                accPref = serviceResult.get("partyAccountingPreference");
            } catch (Exception e) {
                Debug.logError(e, "Error calling getPartyAccountingPreferences: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            inventoryItem.put("currencyUomId", ((Map<String, Object>) accPref).get("baseCurrencyUomId"));
            if (UtilValidate.isEmpty(inventoryItem.get("currencyUomId"))) {
                Object inventoryItem_currencyUomId = UtilProperties.getMessage("general", "currency.uom.id.default", locale);
            }
            if (UtilValidate.isEmpty(inventoryItem.get("currencyUomId"))) {
                {
                    String errorMsg = UtilProperties.getMessage("ProductUiLabels", "FacilityInventoryItemsMissingCurrencyId", locale);
                    error_list.add(errorMsg);
                }
                if (!error_list.isEmpty()) {
                    return ServiceUtil.returnError(error_list);
                }
            }
        }
        if (UtilValidate.isEmpty(inventoryItem.get("unitCost"))) {
            inputMap.put("productId", inventoryItem.get("productId"));
            inputMap.put("currencyUomId", inventoryItem.get("currencyUomId"));
            inputMap.put("costComponentTypePrefix", "EST_STD");
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("getProductCost", inputMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
                inventoryItem.put("unitCost", serviceResult.get("productCost"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling getProductCost: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        if (UtilValidate.isEmpty(inventoryItem.get("unitCost"))) {
            {
                String errorMsg = UtilProperties.getMessage("ProductUiLabels", "FacilityInventoryItemsMissingUnitCost", locale);
                error_list.add(errorMsg);
            }
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        if (((Comparable) inventoryItem.get("unitCost")).compareTo(BigDecimal.ZERO) < 0) {
            {
                String errorMsg = UtilProperties.getMessage("ProductUiLabels", "FacilityInventoryItemsNegativeUnitCost", locale);
                error_list.add(errorMsg);
            }
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        if (UtilValidate.isNotEmpty(updateInventoryItem)) {
            try {
                delegator.store(inventoryItem);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        return result;
    }


    /**
     * Update an InventoryItem
     */
    public static Map<String, Object> updateInventoryItem(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue lookedUpValue = null;
        GenericValue oldFacility = null;
        Map<String, Object> createInventoryItemDetailInMap = null;
        GenericValue lookupPKMap = delegator.makeValue("InventoryItem");
        lookupPKMap.setPKFields(context);
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from(lookupPKMap.getEntityName())
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key : " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(lookedUpValue.get("ownerPartyId"))) {
            try {
                oldFacility = lookedUpValue.getRelatedOne("Facility", false);
            } catch (Exception e) {
                Debug.logError(e, "Error getting related one Facility: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            lookedUpValue.put("ownerPartyId", oldFacility.get("ownerPartyId"));
        }
        result.put("oldOwnerPartyId", lookedUpValue.get("ownerPartyId"));
        result.put("oldStatusId", lookedUpValue.get("statusId"));
        result.put("oldProductId", lookedUpValue.get("productId"));
        if (UtilValidate.isNotEmpty(context.get("unitCost"))) {
            if (((Comparable) context.get("unitCost")).compareTo(new BigDecimal("0.0")) < 0) {
                {
                    String errorMsg = UtilProperties.getMessage("ProductUiLabels", "FacilityInventoryItemsUnitCostCannotBeNegative", locale);
                    error_list.add(errorMsg);
                }
                if (!error_list.isEmpty()) {
                    return ServiceUtil.returnError(error_list);
                }
            }
        }
        Object oldUnitCost = lookedUpValue.get("unitCost");
        lookedUpValue.setNonPKFields(context);
        try {
            delegator.store(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(context.get("unitCost"))) {
            if (!java.util.Objects.equals(context.get("unitCost"), oldUnitCost)) {
                createInventoryItemDetailInMap.put("inventoryItemId", lookedUpValue.get("inventoryItemId"));
                createInventoryItemDetailInMap.put("unitCost", context.get("unitCost"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createInventoryItemDetail", createInventoryItemDetailInMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createInventoryItemDetail: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }

        return result;
    }


    /**
     * Create an inventory item status record
     */
    public static Map<String, Object> createInventoryItemStatus(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue oldInventoryItemStatus = null;
        GenericValue inventoryItem = null;
        GenericValue inventoryItemStatus = null;
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        List<GenericValue> oldInventoryItemStatusList = null;
        try {
            oldInventoryItemStatusList = EntityQuery.use(delegator)
                    .from("InventoryItemStatus")
                    .where(UtilMisc.toMap("inventoryItemId", context.get("inventoryItemId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        oldInventoryItemStatus = EntityUtil.getFirst((List<GenericValue>) oldInventoryItemStatusList);
        if (UtilValidate.isNotEmpty(oldInventoryItemStatus)) {
            oldInventoryItemStatus.put("statusEndDatetime", nowTimestamp);
            try {
                delegator.store(oldInventoryItemStatus);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        inventoryItemStatus = delegator.makeValue("InventoryItemStatus");
        inventoryItemStatus.setNonPKFields(context);
        inventoryItemStatus.setPKFields(context);
        inventoryItemStatus.put("statusDatetime", nowTimestamp);
        inventoryItemStatus.put("changeByUserLoginId", userLogin.get("userLoginId"));
        if (UtilValidate.isEmpty(inventoryItemStatus.get("productId"))) {
            try {
                inventoryItem = EntityQuery.use(delegator)
                        .from("InventoryItem")
                        .where(context)
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying InventoryItem: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            inventoryItemStatus.put("productId", inventoryItem.get("productId"));
        }
        try {
            delegator.create(inventoryItemStatus);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Create an InventoryItemDetail
     */
    public static Map<String, Object> createInventoryItemDetail(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue newEntity = null;
        GenericValue itemIssuance = null;
        newEntity = delegator.makeValue("InventoryItemDetail");
        newEntity.put("inventoryItemId", context.get("inventoryItemId"));
        ((GenericValue) newEntity).put("inventoryItemDetailSeqId", delegator.getNextSeqId("InventoryItemDetail"));
        result.put("inventoryItemDetailSeqId", newEntity.get("inventoryItemDetailSeqId"));
        newEntity.setNonPKFields(context);
        if (UtilValidate.isNotEmpty(context.get("itemIssuanceId"))) {
            try {
                itemIssuance = EntityQuery.use(delegator)
                        .from("ItemIssuance")
                        .where(context)
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ItemIssuance: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            newEntity.put("effectiveDate", itemIssuance.get("issuedDateTime"));
        } else {
            Timestamp newEntity_effectiveDate = new Timestamp(System.currentTimeMillis());
        }
        if (UtilValidate.isEmpty(newEntity.get("availableToPromiseDiff"))) {
            newEntity.put("availableToPromiseDiff", BigDecimal.ZERO);
        }
        if (UtilValidate.isEmpty(newEntity.get("quantityOnHandDiff"))) {
            newEntity.put("quantityOnHandDiff", BigDecimal.ZERO);
        }
        if (UtilValidate.isEmpty(newEntity.get("accountingQuantityDiff"))) {
            newEntity.put("accountingQuantityDiff", BigDecimal.ZERO);
        }
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Update an InventoryItem From the Associated Detail Records
     */
    public static Map<String, Object> updateInventoryItemFromDetail(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue inventoryItem = null;
        try {
            inventoryItem = EntityQuery.use(delegator)
                    .from("InventoryItem")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying InventoryItem: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue inventoryItemDetailSummary = null;
        try {
            inventoryItemDetailSummary = EntityQuery.use(delegator)
                    .from("InventoryItemDetailSummary")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying InventoryItemDetailSummary: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        inventoryItem.put("availableToPromiseTotal", inventoryItemDetailSummary.get("availableToPromiseTotal"));
        inventoryItem.put("quantityOnHandTotal", inventoryItemDetailSummary.get("quantityOnHandTotal"));
        inventoryItem.put("accountingQuantityTotal", inventoryItemDetailSummary.get("accountingQuantityTotal"));
        try {
            delegator.store(inventoryItem);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Update the totals on serialized inventory
     */
    public static Map<String, Object> updateSerializedInventoryTotals(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue inventoryItem = null;
        try {
            inventoryItem = EntityQuery.use(delegator)
                    .from("InventoryItem")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying InventoryItem: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if ("SERIALIZED_INV_ITEM".equals(inventoryItem.get("inventoryItemTypeId"))) {
            BigDecimal inventoryItem_availableToPromiseTotal = null;
            BigDecimal inventoryItem_quantityOnHandTotal = null;
            if (("INV_AVAILABLE".equals(inventoryItem.get("statusId")) && (!java.util.Objects.equals(inventoryItem.get("availableToPromiseTotal"), BigDecimal.ONE) || !java.util.Objects.equals(inventoryItem.get("quantityOnHandTotal"), BigDecimal.ONE)))) {
                inventoryItem.put("availableToPromiseTotal", BigDecimal.ONE);
                inventoryItem.put("quantityOnHandTotal", BigDecimal.ONE);
                Debug.logInfo("In updateSerializedInventoryTotals Storing totals for item [" + inventoryItem.get("inventoryItemId") + "] INV_AVAIABLE [1/1]", MODULE);
                try {
                    delegator.store(inventoryItem);
                } catch (Exception e) {
                    Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            } else if (("INV_DELIVERED".equals(inventoryItem.get("statusId")) && (!java.util.Objects.equals(inventoryItem.get("availableToPromiseTotal"), BigDecimal.ZERO) || !java.util.Objects.equals(inventoryItem.get("quantityOnHandTotal"), BigDecimal.ZERO)))) {
                inventoryItem.put("availableToPromiseTotal", BigDecimal.ZERO);
                inventoryItem.put("quantityOnHandTotal", BigDecimal.ZERO);
                Debug.logInfo("In updateSerializedInventoryTotals Storing totals [" + inventoryItem.get("inventoryItemId") + "] for INV_DELIVERED [0/0]", MODULE);
                try {
                    delegator.store(inventoryItem);
                } catch (Exception e) {
                    Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            } else if ((!"INV_AVAILABLE".equals(inventoryItem.get("statusId")) && !"INV_DELIVERED".equals(inventoryItem.get("statusId")) && (!java.util.Objects.equals(inventoryItem.get("availableToPromiseTotal"), BigDecimal.ZERO) || !java.util.Objects.equals(inventoryItem.get("quantityOnHandTotal"), BigDecimal.ONE)))) {
                inventoryItem.put("availableToPromiseTotal", BigDecimal.ZERO);
                inventoryItem.put("quantityOnHandTotal", BigDecimal.ONE);
                Debug.logInfo("In updateSerializedInventoryTotals Storing totals [" + inventoryItem.get("inventoryItemId") + "] for other status [0/1]", MODULE);
                try {
                    delegator.store(inventoryItem);
                } catch (Exception e) {
                    Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }

        return result;
    }


    /**
     * Update Old Inventory To Detail All
     */
    public static Map<String, Object> updateOldInventoryToDetailAll(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> callServiceMap = null;
        List<GenericValue> inventoryItemList = null;
        try {
            inventoryItemList = EntityQuery.use(delegator)
                    .from("InventoryItem")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying InventoryItem: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (inventoryItemList != null) {
            for (GenericValue inventoryItem : inventoryItemList) {
                callServiceMap.put("inventoryItem", inventoryItem);
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("updateOldInventoryToDetailSingle", callServiceMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling updateOldInventoryToDetailSingle: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                callServiceMap.remove("inventoryItem");
            }
        }

        return result;
    }


    /**
     * Update Old Inventory To Detail Single
     */
    public static Map<String, Object> updateOldInventoryToDetailSingle(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> createDetailMap = new HashMap<String, Object>();
        createDetailMap.put("inventoryItemId", ((Map<String, Object>) context.get("inventoryItem")).get("inventoryItemId"));
        createDetailMap.put("availableToPromiseDiff", ((Map<String, Object>) context.get("inventoryItem")).get("oldAvailableToPromise"));
        createDetailMap.put("quantityOnHandDiff", ((Map<String, Object>) context.get("inventoryItem")).get("oldQuantityOnHand"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createInventoryItemDetail", createDetailMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createInventoryItemDetail: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        context.remove("inventoryItem.oldAvailableToPromise");
        context.remove("inventoryItem.oldQuantityOnHand");
        try {
            delegator.store((GenericValue) context.get("inventoryItem"));
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Check Product Inventory Discontinuation
     */
    public static Map<String, Object> checkProductInventoryDiscontinuation(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue product = null;
        Map<String, Object> getAssoc = null;
        GenericValue assoc = null;
        GenericValue virtProduct = null;
        List<GenericValue> assocsDate = null;
        List<GenericValue> assocs = null;
        Map<String, Object> getFromAssoc = null;
        Map<String, Object> discontinueProductSalesMap = null;
        Object availableToPromiseTotal = null;
        Map<String, Object> productIdMap = new HashMap<String, Object>();
        productIdMap.put("productId", context.get("productId"));
        try {
            product = EntityQuery.use(delegator)
                    .from("Product")
                    .where(productIdMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key Product: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        if (UtilValidate.isNotEmpty(product)) {
            if ("Y".equals(product.get("isVariant"))) {
                getAssoc.put("productIdTo", product.get("productId"));
                getAssoc.put("productAssocTypeId", "PRODUCT_VARIANT");
                try {
                    assocs = EntityQuery.use(delegator)
                            .from("ProductAssoc")
                            .where(getAssoc)
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying ProductAssoc: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (UtilValidate.isNotEmpty(assocs)) {
                    assocsDate = EntityUtil.filterByDate(UtilGenerics.cast(assocs));
                    assoc = EntityUtil.getFirst((List<GenericValue>) assocsDate);
                    if (UtilValidate.isNotEmpty(assoc)) {
                        try {
                            virtProduct = assoc.getRelatedOne("MainProduct", false);
                        } catch (Exception e) {
                            Debug.logError(e, "Error getting related one MainProduct: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                        if (UtilValidate.isEmpty(product.get("salesDiscWhenNotAvail"))) {
                            product.put("salesDiscWhenNotAvail", virtProduct.get("salesDiscWhenNotAvail"));
                        }
                    }
                }
            }
        }
        Object discontinueProductSalesMap_productId = null;
        Object getFromAssoc_productId = null;
        Object getFromAssoc_productAssocTypeId = null;
        if ((!(UtilValidate.isEmpty(product)) && "Y".equals(product.get("salesDiscWhenNotAvail")) && (UtilValidate.isEmpty(product.get("salesDiscontinuationDate")) || product.get("salesDiscontinuationDate") != null /* TODO: field compare operator greater */))) {
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("getProductInventoryAvailable", productIdMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
                availableToPromiseTotal = serviceResult.get("availableToPromiseTotal");
            } catch (Exception e) {
                Debug.logError(e, "Error calling getProductInventoryAvailable: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (((Comparable) availableToPromiseTotal).compareTo(BigDecimal.ZERO) <= 0) {
                discontinueProductSalesMap.put("productId", context.get("productId"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("discontinueProductSales", discontinueProductSalesMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling discontinueProductSales: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
            if (UtilValidate.isNotEmpty(virtProduct)) {
                if ("Y".equals(virtProduct.get("salesDiscWhenNotAvail"))) {
                    getFromAssoc.put("productId", virtProduct.get("productId"));
                    getFromAssoc.put("productAssocTypeId", "PRODUCT_VARIANT");
                    try {
                        assocs = EntityQuery.use(delegator)
                                .from("ProductAssoc")
                                .where(getFromAssoc)
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying ProductAssoc: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    assocsDate = EntityUtil.filterByDate(UtilGenerics.cast(assocs));
                    if (UtilValidate.isEmpty(assocsDate)) {
                        discontinueProductSalesMap.put("productId", virtProduct.get("productId"));
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("discontinueProductSales", discontinueProductSalesMap);
                            if (ServiceUtil.isError(serviceResult)) {
                                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                            }
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling discontinueProductSales: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                    }
                }
            }
        }

        return result;
    }


    /**
     * Create an InventoryItemVariance
     */
    public static Map<String, Object> createInventoryItemVariance(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue inventoryItemLookup = delegator.makeValue("InventoryItem");
        inventoryItemLookup.setPKFields(context);
        GenericValue inventoryItem = null;
        try {
            inventoryItem = EntityQuery.use(delegator)
                    .from(inventoryItemLookup.getEntityName())
                    .where(inventoryItemLookup)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key : " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (!"NON_SERIAL_INV_ITEM".equals(inventoryItem.get("inventoryItemTypeId"))) {
            error_list.add("Can only create an InventoryItemVariance for a Non-Serialized Inventory Item");
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        Map<String, Object> createDetailMap = new HashMap<String, Object>();
        createDetailMap.put("inventoryItemId", context.get("inventoryItemId"));
        createDetailMap.put("physicalInventoryId", context.get("physicalInventoryId"));
        createDetailMap.put("availableToPromiseDiff", context.get("availableToPromiseVar"));
        createDetailMap.put("quantityOnHandDiff", context.get("quantityOnHandVar"));
        createDetailMap.put("reasonEnumId", context.get("varianceReasonId"));
        createDetailMap.put("description", context.get("comments"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createInventoryItemDetail", createDetailMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createInventoryItemDetail: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue newEntity = delegator.makeValue("InventoryItemVariance");
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
     * Create a PhysicalInventory
     */
    public static Map<String, Object> createPhysicalInventory(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue newEntity = null;
        newEntity = delegator.makeValue("PhysicalInventory");
        newEntity.setNonPKFields(context);
        if (UtilValidate.isEmpty(newEntity.get("physicalInventoryDate"))) {
            Timestamp newEntity_physicalInventoryDate = new Timestamp(System.currentTimeMillis());
        }
        if (UtilValidate.isEmpty(newEntity.get("partyId"))) {
            newEntity.put("partyId", userLogin.get("partyId"));
        }
        String physicalInventoryId = delegator.getNextSeqId("PhysicalInventory");
        newEntity.put("physicalInventoryId", physicalInventoryId);
        result.put("physicalInventoryId", physicalInventoryId);
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Create a PhysicalInventory and an InventoryItemVariance
     */
    public static Map<String, Object> createPhysicalInventoryAndVariance(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> createPhysicalInventoryMap = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "createPhysicalInventoryMap" for service "createPhysicalInventory"
        createPhysicalInventoryMap.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createPhysicalInventory", createPhysicalInventoryMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            context.put("physicalInventoryId", serviceResult.get("physicalInventoryId"));
            result.put("physicalInventoryId", serviceResult.get("physicalInventoryId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createPhysicalInventory: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> createInventoryItemVarianceMap = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "createInventoryItemVarianceMap" for service "createInventoryItemVariance"
        createInventoryItemVarianceMap.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createInventoryItemVariance", createInventoryItemVarianceMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createInventoryItemVariance: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Create a ProductFacility
     */
    public static Map<String, Object> createProductFacility(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue newEntity = delegator.makeValue("ProductFacility");
        newEntity.setNonPKFields(context);
        newEntity.setPKFields(context);
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Update a ProductFacility
     */
    public static Map<String, Object> updateProductFacility(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookupPKMap = delegator.makeValue("ProductFacility");
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
     * Delete a ProductFacility
     */
    public static Map<String, Object> deleteProductFacility(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookupPKMap = delegator.makeValue("ProductFacility");
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
        try {
            delegator.removeValue(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Create a ProductFacilityLocation
     */
    public static Map<String, Object> createProductFacilityLocation(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue newEntity = delegator.makeValue("ProductFacilityLocation");
        newEntity.setNonPKFields(context);
        newEntity.setPKFields(context);
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Update a ProductFacilityLocation
     */
    public static Map<String, Object> updateProductFacilityLocation(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookupPKMap = delegator.makeValue("ProductFacilityLocation");
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
     * Delete a ProductFacilityLocation
     */
    public static Map<String, Object> deleteProductFacilityLocation(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookupPKMap = delegator.makeValue("ProductFacilityLocation");
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
        try {
            delegator.removeValue(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Get Inventory Available for a Product
     */
    public static Map<String, Object> getProductInventoryAvailable(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> inlineResult = null;
        Map<String, Object> lookupFieldMap = null;
        List<GenericValue> inventoryItems = null;
        List<GenericValue> productFacilities = null;
        GenericValue productFacility = null;
        Map<String, Object> pfLookupMap = null;
        if (Boolean.TRUE.equals(context.get("useInventoryCache"))) {
            if ((UtilValidate.isEmpty(context.get("inventoryItemId")) && UtilValidate.isEmpty(context.get("partyId")) && UtilValidate.isEmpty(context.get("locationSeqId")) && UtilValidate.isEmpty(context.get("containerId")) && UtilValidate.isEmpty(context.get("lotId")))) {
                inlineResult = getProductInventoryAvailableCached(dctx, context);
                if (ServiceUtil.isError(inlineResult)) {
                    return inlineResult;
                }
                return result;
            } else {
                Debug.logWarning("Ignoring useInventoryCache true because unsupported parameters passed (inventoryItemId/partyId/locationSeqId/containerId/lotId)", MODULE);
            }
        }
        if (UtilValidate.isEmpty(context.get("useCache"))) {
            context.put("useCache", context.get("useEntityCache"));
        }
        if ("nullField".equals(context.get("locationSeqId"))) {
            lookupFieldMap.put("locationSeqId", null);
        }
        lookupFieldMap.put("inventoryItemId", context.get("inventoryItemId"));
        lookupFieldMap.put("productId", context.get("productId"));
        lookupFieldMap.put("facilityId", context.get("facilityId"));
        lookupFieldMap.put("partyId", context.get("partyId"));
        lookupFieldMap.put("locationSeqId", context.get("locationSeqId"));
        lookupFieldMap.put("containerId", context.get("containerId"));
        lookupFieldMap.put("lotId", context.get("lotId"));
        if (Boolean.TRUE.equals(context.get("useCache"))) {
            try {
                inventoryItems = EntityQuery.use(delegator)
                        .from("InventoryItem")
                        .where(lookupFieldMap)
                        .cache()
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying InventoryItem: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        } else {
            try {
                inventoryItems = EntityQuery.use(delegator)
                        .from("InventoryItem")
                        .where(lookupFieldMap)
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying InventoryItem: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        context.put("availableToPromiseTotal", BigDecimal.ZERO);
        context.put("quantityOnHandTotal", BigDecimal.ZERO);
        if (inventoryItems != null) {
            for (GenericValue inventoryItem : inventoryItems) {
                BigDecimal parameters_quantityOnHandTotal = null;
                BigDecimal parameters_availableToPromiseTotal = null;
                if (((!(UtilValidate.isEmpty(context.get("statusId"))) && java.util.Objects.equals(context.get("statusId"), inventoryItem.get("statusId"))) || (UtilValidate.isEmpty(context.get("statusId")) && (UtilValidate.isEmpty(inventoryItem.get("statusId")) || "INV_AVAILABLE".equals(inventoryItem.get("statusId")) || "INV_NS_RETURNED".equals(inventoryItem.get("statusId")) || "SERIALIZED_INV_ITEM".equals(inventoryItem.get("inventoryItemTypeId")))))) {
                    context.put("quantityOnHandTotal", (BigDecimal) ((BigDecimal) context.get("quantityOnHandTotal")).add((BigDecimal) inventoryItem.get("quantityOnHandTotal")));
                    context.put("availableToPromiseTotal", (BigDecimal) ((BigDecimal) context.get("availableToPromiseTotal")).add((BigDecimal) inventoryItem.get("availableToPromiseTotal")));
                }
            }
        }
        result.put("availableToPromiseTotal", context.get("availableToPromiseTotal"));
        result.put("quantityOnHandTotal", context.get("quantityOnHandTotal"));

        return result;
    }


    /**
     * Get Inventory Available for a Product from inventory Cache (SCIPIO)
     */
    public static Map<String, Object> getProductInventoryAvailableCached(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<GenericValue> productFacilities = null;
        if (UtilValidate.isEmpty(context.get("useCache"))) {
            context.put("useCache", context.get("useEntityCache"));
        }
        Map<String, Object> pfLookupMap = new HashMap<String, Object>();
        pfLookupMap.put("productId", context.get("productId"));
        pfLookupMap.put("facilityId", context.get("facilityId"));
        if (Boolean.TRUE.equals(context.get("useCache"))) {
            try {
                productFacilities = EntityQuery.use(delegator)
                        .from("ProductFacility")
                        .where(pfLookupMap)
                        .cache()
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ProductFacility: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        } else {
            try {
                productFacilities = EntityQuery.use(delegator)
                        .from("ProductFacility")
                        .where(pfLookupMap)
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ProductFacility: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        context.put("availableToPromiseTotal", BigDecimal.ZERO);
        context.put("quantityOnHandTotal", BigDecimal.ZERO);
        if (productFacilities != null) {
            for (GenericValue productFacility : productFacilities) {
                if (UtilValidate.isNotEmpty(productFacility.get("lastInventoryCountQoh"))) {
                    context.put("quantityOnHandTotal", (BigDecimal) ((BigDecimal) context.get("quantityOnHandTotal")).add((BigDecimal) productFacility.get("lastInventoryCountQoh")));
                }
                context.put("availableToPromiseTotal", (BigDecimal) ((BigDecimal) context.get("availableToPromiseTotal")).add((BigDecimal) productFacility.get("lastInventoryCount")));
            }
        }
        result.put("availableToPromiseTotal", context.get("availableToPromiseTotal"));
        result.put("quantityOnHandTotal", context.get("quantityOnHandTotal"));

        return result;
    }


    /**
     * Count Inventory On Hand for a Product constrained by a facilityId at a given date.
     */
    public static Map<String, Object> countProductInventoryOnHand(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<GenericValue> inventoryItemDetailTotals = null;
        try {
            inventoryItemDetailTotals = EntityQuery.use(delegator)
                    .from("InventoryItemDetailForSum")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying InventoryItemDetailForSum: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue inventoryItemDetailTotal = EntityUtil.getFirst((List<GenericValue>) inventoryItemDetailTotals);
        BigDecimal quantityOnHandTotal = (BigDecimal) inventoryItemDetailTotal.get("quantityOnHandSum");
        result.put("quantityOnHandTotal", quantityOnHandTotal);

        return result;
    }


    /**
     * Count Inventory Shipped for Sales Orders for a Product constrained by a facilityId in a given date range.
     */
    public static Map<String, Object> countProductInventoryShippedForSales(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        if (UtilValidate.isEmpty(context.get("thruDate"))) {
            Timestamp parameters_thruDate = new Timestamp(System.currentTimeMillis());
        }
        List<GenericValue> inventoryItemDetailTotals = null;
        try {
            inventoryItemDetailTotals = EntityQuery.use(delegator)
                    .from("InventoryItemDetailForSum")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying InventoryItemDetailForSum: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue inventoryItemDetailTotal = EntityUtil.getFirst((List<GenericValue>) inventoryItemDetailTotals);
        BigDecimal quantityOnHandTotal = (BigDecimal) ((Map<String, Object>) context.get("${inventoryItemDetailTotal")).get("quantityOnHandSum * -1}");
        result.put("quantityOnHandTotal", quantityOnHandTotal);

        return result;
    }


    /**
     * Get Marketing Packages Available From Components In Inventory
     */
    public static Map<String, Object> getMktgPackagesAvailable(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Object quantityOnHandTotal = null;
        Map<String, Object> inventoryByAssocProductsParams = null;
        Map<String, Object> productIdMap = null;
        Object availableToPromiseTotal = null;
        Object assocProducts = null;
        Map<String, Object> lookupMktgPkgParams = null;
        availableToPromiseTotal = BigDecimal.ZERO;
        quantityOnHandTotal = BigDecimal.ZERO;
        lookupMktgPkgParams.put("productId", context.get("productId"));
        GenericValue product = null;
        try {
            product = EntityQuery.use(delegator)
                    .from("Product")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Product: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Object isMarketingPkgAuto = (Boolean) GroovyUtil.eval("org.ofbiz.entity.util.EntityTypeUtil.hasParentType(delegator, 'ProductType', 'productTypeId', product.productTypeId, 'parentTypeId', 'MARKETING_PKG_AUTO')", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
        if (Boolean.TRUE.equals(isMarketingPkgAuto)) {
            productIdMap.put("productId", product.get("productId"));
            productIdMap.put("useInventoryCache", context.get("useInventoryCache"));
            productIdMap.put("useEntityCache", context.get("useEntityCache"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("getProductInventoryAvailable", productIdMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
                availableToPromiseTotal = serviceResult.get("availableToPromiseTotal");
                quantityOnHandTotal = serviceResult.get("quantityOnHandTotal");
            } catch (Exception e) {
                Debug.logError(e, "Error calling getProductInventoryAvailable: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        } else {
            lookupMktgPkgParams.put("type", "PRODUCT_COMPONENT");
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("getAssociatedProducts", lookupMktgPkgParams);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
                assocProducts = serviceResult.get("assocProducts");
            } catch (Exception e) {
                Debug.logError(e, "Error calling getAssociatedProducts: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (UtilValidate.isNotEmpty(assocProducts)) {
                inventoryByAssocProductsParams.put("assocProducts", assocProducts);
                inventoryByAssocProductsParams.put("facilityId", context.get("facilityId"));
                inventoryByAssocProductsParams.put("statusId", context.get("statusId"));
                inventoryByAssocProductsParams.put("useInventoryCache", context.get("useInventoryCache"));
                inventoryByAssocProductsParams.put("useEntityCache", context.get("useEntityCache"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("getProductInventoryAvailableFromAssocProducts", inventoryByAssocProductsParams);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                    quantityOnHandTotal = serviceResult.get("quantityOnHandTotal");
                    availableToPromiseTotal = serviceResult.get("availableToPromiseTotal");
                } catch (Exception e) {
                    Debug.logError(e, "Error calling getProductInventoryAvailableFromAssocProducts: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }
        result.put("availableToPromiseTotal", availableToPromiseTotal);
        result.put("quantityOnHandTotal", quantityOnHandTotal);

        return result;
    }


    /**
     * Balances available-to-promise on inventory items
     */
    public static Map<String, Object> balanceInventoryItems(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue inventoryItem = null;
        try {
            inventoryItem = EntityQuery.use(delegator)
                    .from("InventoryItem")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying InventoryItem: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> reassignInventoryReservationsCtx = new HashMap<String, Object>();
        reassignInventoryReservationsCtx.put("productId", inventoryItem.get("productId"));
        reassignInventoryReservationsCtx.put("facilityId", inventoryItem.get("facilityId"));
        reassignInventoryReservationsCtx.put("fromDate", context.get("nowTimestamp"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("reassignInventoryReservations", reassignInventoryReservationsCtx);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling reassignInventoryReservations: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Balances available-to-promise on inventory items
     */
    public static Map<String, Object> reassignInventoryReservations(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<Object> privilegedReservations = null;
        List<Object> reservations = null;
        List<GenericValue> picklistItemList = null;
        GenericValue oisgir = null;
        Object cancelOisgirMap = null;
        Object resMap = null;
        Map<String, Object> touchedOrderIdMap = null;
        GenericValue orderHeader = null;
        Object isBackOrder = null;
        Map<String, Object> noLongerOnBackOrderIdMap = null;
        Map<String, Object> checkOrderIsOnBackOrderMap = null;
        Object noLongerOnBackOrderIdSet = null;
        List<GenericValue> relatedRes = null;
        try {
            relatedRes = EntityQuery.use(delegator)
                    .from("OrderItemShipGrpInvResAndItem")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderItemShipGrpInvResAndItem: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (relatedRes != null) {
            for (GenericValue oneRelatedRes : relatedRes) {
                try {
                    picklistItemList = EntityQuery.use(delegator)
                            .from("PicklistAndBinAndItem")
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying PicklistAndBinAndItem: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (UtilValidate.isEmpty(picklistItemList)) {
                    Debug.logInfo("Order [" + oneRelatedRes.get("orderId") + "] was not found on any picklist for InventoryItem [" + oneRelatedRes.get("inventoryItemId") + "]", MODULE);
                    if ((java.util.Objects.equals(context.get("priorityOrderId"), oneRelatedRes.get("orderId")) && java.util.Objects.equals(context.get("priorityOrderItemSeqId"), oneRelatedRes.get("orderItemSeqId")))) {
                        privilegedReservations.add(oneRelatedRes);
                    } else {
                        reservations.add(oneRelatedRes);
                    }
                }
            }
        }
        List<Object> allReservations = new LinkedList<>();
        allReservations.addAll(privilegedReservations);
        allReservations.addAll(reservations);
        if (allReservations != null) {
            for (Object oisgir_iter : allReservations) {
                oisgir = (GenericValue) oisgir_iter;
                cancelOisgirMap = new HashMap<String, Object>();
                ((Map<String, Object>) cancelOisgirMap).put("orderId", oisgir.get("orderId"));
                ((Map<String, Object>) cancelOisgirMap).put("orderItemSeqId", oisgir.get("orderItemSeqId"));
                ((Map<String, Object>) cancelOisgirMap).put("inventoryItemId", oisgir.get("inventoryItemId"));
                ((Map<String, Object>) cancelOisgirMap).put("shipGroupSeqId", oisgir.get("shipGroupSeqId"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("cancelOrderItemShipGrpInvRes", (Map<String, Object>) cancelOisgirMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling cancelOrderItemShipGrpInvRes: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }
        if (allReservations != null) {
            for (Object oisgir_iter : allReservations) {
                oisgir = (GenericValue) oisgir_iter;
                if (UtilValidate.isNotEmpty(oisgir.get("quantityNotAvailable"))) {
                    if (((Comparable) oisgir.get("quantityNotAvailable")).compareTo(BigDecimal.ZERO) > 0) {
                        touchedOrderIdMap.put((String) oisgir.get("orderId"), "Y");
                        Debug.logVerbose("Adding " + oisgir.get("orderId") + " to touchedOrderIdMap", MODULE);
                    }
                }
                try {
                    orderHeader = EntityQuery.use(delegator)
                            .from("OrderHeader")
                            .where(UtilMisc.toMap("orderId", oisgir.get("orderId")))
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying OrderHeader: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                resMap = new HashMap<String, Object>();
                ((Map<String, Object>) resMap).put("productId", context.get("productId"));
                ((Map<String, Object>) resMap).put("orderId", oisgir.get("orderId"));
                ((Map<String, Object>) resMap).put("orderItemSeqId", oisgir.get("orderItemSeqId"));
                ((Map<String, Object>) resMap).put("quantity", oisgir.get("quantity"));
                ((Map<String, Object>) resMap).put("reservedDatetime", oisgir.get("reservedDatetime"));
                ((Map<String, Object>) resMap).put("reserveOrderEnumId", oisgir.get("reserveOrderEnumId"));
                ((Map<String, Object>) resMap).put("requireInventory", "N");
                ((Map<String, Object>) resMap).put("shipGroupSeqId", oisgir.get("shipGroupSeqId"));
                ((Map<String, Object>) resMap).put("sequenceId", oisgir.get("sequenceId"));
                ((Map<String, Object>) resMap).put("facilityId", context.get("facilityId"));
                ((Map<String, Object>) resMap).put("priority", orderHeader.get("priority"));
                Debug.logInfo("Re-reserving product [" + ((Map<String, Object>) resMap).get("productId") + "] for order item [" + ((Map<String, Object>) resMap).get("orderId") + ":" + ((Map<String, Object>) resMap).get("orderItemSeqId") + "] quantity [" + ((Map<String, Object>) resMap).get("quantity") + "]; facility [" + context.get("facilityId") + "]", MODULE);
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("reserveProductInventoryByFacility", (Map<String, Object>) resMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling reserveProductInventoryByFacility: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }
        for (Map.Entry<String, Object> entry : ((Map<String, Object>) touchedOrderIdMap).entrySet()) {
            String touchedOrderId = entry.getKey();
            Object throwAwayValue = entry.getValue();
            checkOrderIsOnBackOrderMap.put("orderId", touchedOrderId);
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("checkOrderIsOnBackOrder", checkOrderIsOnBackOrderMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
                isBackOrder = serviceResult.get("isBackOrder");
            } catch (Exception e) {
                Debug.logError(e, "Error calling checkOrderIsOnBackOrder: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (Boolean.FALSE.equals(isBackOrder)) {
                noLongerOnBackOrderIdMap.put((String) touchedOrderId, "Y");
            }
        }
        if (UtilValidate.isNotEmpty(noLongerOnBackOrderIdMap)) {
            noLongerOnBackOrderIdSet = noLongerOnBackOrderIdMap.keySet();
            result.put("noLongerOnBackOrderIdSet", noLongerOnBackOrderIdSet);
        }

        return result;
    }


    /**
     * To balance order items with negative reservations
     */
    public static Map<String, Object> balanceOrderItemsWithNegativeReservations(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<GenericValue> oisgirais = null;
        GenericValue oisgir = null;
        Map<String, Object> reassignInventoryReservationsCtx = new HashMap<>();
        Map<String, Object> orderItems = null;
        Timestamp nowTimestamp = null;
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
            productStore = orderHeader.getRelatedOne("ProductStore", false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related one ProductStore: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if ("Y".equals(productStore.get("balanceResOnOrderCreation"))) {
            try {
                oisgirais = EntityQuery.use(delegator)
                        .from("OrderItemAndShipGrpInvResAndItem")
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying OrderItemAndShipGrpInvResAndItem: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (oisgirais != null) {
                for (GenericValue oisgir_iter : oisgirais) {
                    oisgir = oisgir_iter;
                    orderItems.put((String) oisgir.get("orderItemSeqId"), oisgir);
                }
            }
            nowTimestamp = new Timestamp(System.currentTimeMillis());
            for (Map.Entry<String, Object> entry : ((Map<String, Object>) orderItems).entrySet()) {
                String orderItemSeqId = entry.getKey();
                oisgir = (GenericValue) entry.getValue();
                reassignInventoryReservationsCtx.put("productId", oisgir.get("productId"));
                reassignInventoryReservationsCtx.put("facilityId", oisgir.get("facilityId"));
                if (UtilValidate.isNotEmpty(oisgir.get("shipBeforeDate"))) {
                    reassignInventoryReservationsCtx.put("fromDate", oisgir.get("shipBeforeDate"));
                } else {
                    reassignInventoryReservationsCtx.put("fromDate", nowTimestamp);
                }
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("reassignInventoryReservations", reassignInventoryReservationsCtx);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling reassignInventoryReservations: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        } else {
            Debug.logInfo("Not reassigning the reservations because productStore.balanceResOnOrderCreation is set to N or null.", MODULE);
        }

        return result;
    }


    /**
     * Create an Inventory Transfer
     */
    public static Map<String, Object> createInventoryTransfer(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue newEntity = delegator.makeValue("InventoryTransfer");
        newEntity.setNonPKFields(context);
        ((GenericValue) newEntity).put("inventoryTransferId", delegator.getNextSeqId("InventoryTransfer"));
        result.put("inventoryTransferId", newEntity.get("inventoryTransferId"));
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Update an Inventory Transfer
     */
    public static Map<String, Object> updateInventoryTransfer(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue checkStatusValidChange = null;
        Map<String, Object> lookupPKMap = new HashMap<String, Object>();
        lookupPKMap.put("inventoryTransferId", context.get("inventoryTransferId"));
        GenericValue inventoryTransfer = null;
        try {
            inventoryTransfer = EntityQuery.use(delegator)
                    .from("InventoryTransfer")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key InventoryTransfer: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(context.get("statusId"))) {
            if (!java.util.Objects.equals(context.get("statusId"), inventoryTransfer.get("statusId"))) {
                try {
                    checkStatusValidChange = EntityQuery.use(delegator)
                            .from("StatusValidChange")
                            .where(UtilMisc.toMap("statusId", inventoryTransfer.get("statusId"), "statusIdTo", context.get("statusId")))
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying StatusValidChange: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (UtilValidate.isEmpty(checkStatusValidChange)) {
                    error_list.add("ERROR: Changing the status from " + inventoryTransfer.get("statusId") + " to " + context.get("statusId") + " is not allowed.");
                }
                if (!error_list.isEmpty()) {
                    return ServiceUtil.returnError(error_list);
                }
            }
        }
        inventoryTransfer.setNonPKFields(context);
        try {
            delegator.store(inventoryTransfer);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Create inventory transfers for the given product and quantity. Return the units not available for transfers.
     */
    public static Map<String, Object> createInventoryTransfersForProduct(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<Object> orderByList = null;
        Object quantityNotTransferred = null;
        List<GenericValue> inventoryItems = null;
        Object orderByString = null;
        List<GenericValue> inventoryItemAndLocations = null;
        GenericValue inventoryItemAndLocation = null;
        Object inputMap = null;
        Map<String, Object> lookupFieldMap = new HashMap<String, Object>();
        lookupFieldMap.put("productId", context.get("productId"));
        lookupFieldMap.put("facilityId", context.get("facilityId"));
        lookupFieldMap.put("containerId", context.get("containerId"));
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        GenericValue product = null;
        try {
            product = EntityQuery.use(delegator)
                    .from("Product")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Product: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue facility = null;
        try {
            facility = EntityQuery.use(delegator)
                    .from("Facility")
                    .where(context)
                    .cache()
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Facility: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue productType = null;
        try {
            productType = product.getRelatedOne("ProductType", false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related one ProductType: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue inventoryItem = null;
        if ("N".equals(productType.get("isPhysical"))) {
            quantityNotTransferred = BigDecimal.ZERO;
        } else {
            if ("INVRO_GUNIT_COST".equals(context.get("reserveOrderEnumId"))) {
                orderByString = "unitCost DESC";
            } else {
                if ("INVRO_LUNIT_COST".equals(context.get("reserveOrderEnumId"))) {
                    orderByString = "+unitCost";
                } else {
                    if ("INVRO_FIFO_EXP".equals(context.get("reserveOrderEnumId"))) {
                        orderByString = "+expireDate";
                    } else {
                        if ("INVRO_LIFO_EXP".equals(context.get("reserveOrderEnumId"))) {
                            orderByString = "-expireDate";
                        } else {
                            if ("INVRO_LIFO_REC".equals(context.get("reserveOrderEnumId"))) {
                                orderByString = "-datetimeReceived";
                            } else {
                                orderByString = "+datetimeReceived";
                                context.put("reserveOrderEnumId", "INVRO_FIFO_REC");
                            }
                        }
                    }
                }
            }
            orderByList.add(orderByString);
            quantityNotTransferred = context.get("quantity");
            try {
                inventoryItemAndLocations = EntityQuery.use(delegator)
                        .from("InventoryItemAndLocation")
                        .where(lookupFieldMap)
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying InventoryItemAndLocation: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (inventoryItemAndLocations != null) {
                for (GenericValue inventoryItemAndLocation_iter : inventoryItemAndLocations) {
                    inventoryItemAndLocation = inventoryItemAndLocation_iter;
                    Object inputMap_inventoryItemId = null;
                    Object inputMap_statusId = null;
                    Object inputMap_facilityId = null;
                    Object inputMap_facilityIdTo = null;
                    Object inputMap_sendDate = null;
                    Object inputMap_xferQty = null;
                    if (("FLT_PICKLOC".equals(inventoryItemAndLocation.get("locationTypeEnumId")) && ((Comparable) quantityNotTransferred).compareTo(new BigDecimal("0.0")) > 0 && ((Comparable) inventoryItemAndLocation.get("availableToPromiseTotal")).compareTo(new BigDecimal("0.0")) > 0)) {
                        inputMap = new HashMap<String, Object>();
                        ((Map<String, Object>) inputMap).put("inventoryItemId", inventoryItemAndLocation.get("inventoryItemId"));
                        ((Map<String, Object>) inputMap).put("statusId", "IXF_REQUESTED");
                        ((Map<String, Object>) inputMap).put("facilityId", context.get("facilityId"));
                        ((Map<String, Object>) inputMap).put("facilityIdTo", context.get("facilityIdTo"));
                        ((Map<String, Object>) inputMap).put("sendDate", context.get("sendDate"));
                        if ("NON_SERIAL_INV_ITEM".equals(inventoryItemAndLocation.get("inventoryItemTypeId"))) {
                            if (quantityNotTransferred != null /* TODO: field compare operator greater */) {
                                ((Map<String, Object>) inputMap).put("xferQty", inventoryItemAndLocation.get("availableToPromiseTotal"));
                            } else {
                                ((Map<String, Object>) inputMap).put("xferQty", quantityNotTransferred);
                            }
                            try {
                                Map<String, Object> serviceResult = dispatcher.runSync("createInventoryTransfer", (Map<String, Object>) inputMap);
                                if (ServiceUtil.isError(serviceResult)) {
                                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                                }
                            } catch (Exception e) {
                                Debug.logError(e, "Error calling createInventoryTransfer: " + e.getMessage(), MODULE);
                                return ServiceUtil.returnError(e.getMessage());
                            }
                            quantityNotTransferred = new BigDecimal(((Map<String, Object>) inputMap).get("xferQty").toString());
                        }
                    }
                }
            }
            if (((Comparable) quantityNotTransferred).compareTo(BigDecimal.ZERO) > 0) {
                try {
                    inventoryItemAndLocations = EntityQuery.use(delegator)
                            .from("InventoryItemAndLocation")
                            .where(lookupFieldMap)
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying InventoryItemAndLocation: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (inventoryItemAndLocations != null) {
                    for (GenericValue inventoryItemAndLocation_iter : inventoryItemAndLocations) {
                        inventoryItemAndLocation = inventoryItemAndLocation_iter;
                        if (("FLT_BULK".equals(inventoryItemAndLocation.get("locationTypeEnumId")) && ((Comparable) quantityNotTransferred).compareTo(new BigDecimal("0.0")) > 0 && ((Comparable) inventoryItemAndLocation.get("availableToPromiseTotal")).compareTo(new BigDecimal("0.0")) > 0)) {
                            inputMap = new HashMap<String, Object>();
                            ((Map<String, Object>) inputMap).put("inventoryItemId", inventoryItemAndLocation.get("inventoryItemId"));
                            ((Map<String, Object>) inputMap).put("statusId", "IXF_REQUESTED");
                            ((Map<String, Object>) inputMap).put("facilityId", context.get("facilityId"));
                            ((Map<String, Object>) inputMap).put("facilityIdTo", context.get("facilityIdTo"));
                            ((Map<String, Object>) inputMap).put("sendDate", context.get("sendDate"));
                            if ("NON_SERIAL_INV_ITEM".equals(inventoryItemAndLocation.get("inventoryItemTypeId"))) {
                                if (quantityNotTransferred != null /* TODO: field compare operator greater */) {
                                    ((Map<String, Object>) inputMap).put("xferQty", inventoryItemAndLocation.get("availableToPromiseTotal"));
                                } else {
                                    ((Map<String, Object>) inputMap).put("xferQty", quantityNotTransferred);
                                }
                                try {
                                    Map<String, Object> serviceResult = dispatcher.runSync("createInventoryTransfer", (Map<String, Object>) inputMap);
                                    if (ServiceUtil.isError(serviceResult)) {
                                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                                    }
                                } catch (Exception e) {
                                    Debug.logError(e, "Error calling createInventoryTransfer: " + e.getMessage(), MODULE);
                                    return ServiceUtil.returnError(e.getMessage());
                                }
                                quantityNotTransferred = new BigDecimal(((Map<String, Object>) inputMap).get("xferQty").toString());
                            }
                        }
                    }
                }
            }
            if (((Comparable) quantityNotTransferred).compareTo(BigDecimal.ZERO) > 0) {
                try {
                    inventoryItems = EntityQuery.use(delegator)
                            .from("InventoryItem")
                            .where(lookupFieldMap)
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying InventoryItem: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (inventoryItems != null) {
                    for (GenericValue inventoryItemEntry : inventoryItems) {
                        if ((UtilValidate.isEmpty(inventoryItemEntry.get("locationSeqId")) && ((Comparable) quantityNotTransferred).compareTo(new BigDecimal("0.0")) > 0 && ((Comparable) inventoryItemEntry.get("availableToPromiseTotal")).compareTo(new BigDecimal("0.0")) > 0)) {
                            inputMap = new HashMap<String, Object>();
                            ((Map<String, Object>) inputMap).put("inventoryItemId", inventoryItemEntry.get("inventoryItemId"));
                            ((Map<String, Object>) inputMap).put("statusId", "IXF_REQUESTED");
                            ((Map<String, Object>) inputMap).put("facilityId", context.get("facilityId"));
                            ((Map<String, Object>) inputMap).put("facilityIdTo", context.get("facilityIdTo"));
                            ((Map<String, Object>) inputMap).put("sendDate", context.get("sendDate"));
                            if ("NON_SERIAL_INV_ITEM".equals(inventoryItemEntry.get("inventoryItemTypeId"))) {
                                if (quantityNotTransferred != null /* TODO: field compare operator greater */) {
                                    ((Map<String, Object>) inputMap).put("xferQty", inventoryItemEntry.get("availableToPromiseTotal"));
                                } else {
                                    ((Map<String, Object>) inputMap).put("xferQty", quantityNotTransferred);
                                }
                                try {
                                    Map<String, Object> serviceResult = dispatcher.runSync("createInventoryTransfer", (Map<String, Object>) inputMap);
                                    if (ServiceUtil.isError(serviceResult)) {
                                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                                    }
                                } catch (Exception e) {
                                    Debug.logError(e, "Error calling createInventoryTransfer: " + e.getMessage(), MODULE);
                                    return ServiceUtil.returnError(e.getMessage());
                                }
                                quantityNotTransferred = new BigDecimal(((Map<String, Object>) inputMap).get("xferQty").toString());
                            }
                        }
                    }
                }
            }
        }
        result.put("quantityNotTransferred", quantityNotTransferred);

        return result;
    }


    /**
     * Create an InventoryItemLabelType
     */
    public static Map<String, Object> createInventoryItemLabelType(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue newEntity = delegator.makeValue("InventoryItemLabelType");
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
     * Update an InventoryItemLabelType
     */
    public static Map<String, Object> updateInventoryItemLabelType(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("InventoryItemLabelType")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying InventoryItemLabelType: " + e.getMessage(), MODULE);
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
     * Delete an InventoryItemLabelType
     */
    public static Map<String, Object> deleteInventoryItemLabelType(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("InventoryItemLabelType")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying InventoryItemLabelType: " + e.getMessage(), MODULE);
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
     * Create an InventoryItemLabel
     */
    public static Map<String, Object> createInventoryItemLabel(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue newEntity = delegator.makeValue("InventoryItemLabel");
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
     * Update an InventoryItemLabel
     */
    public static Map<String, Object> updateInventoryItemLabel(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("InventoryItemLabel")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying InventoryItemLabel: " + e.getMessage(), MODULE);
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
     * Delete an InventoryItemLabel
     */
    public static Map<String, Object> deleteInventoryItemLabel(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("InventoryItemLabel")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying InventoryItemLabel: " + e.getMessage(), MODULE);
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
     * Create an InventoryItemLabelAppl
     */
    public static Map<String, Object> createInventoryItemLabelAppl(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue newEntity = delegator.makeValue("InventoryItemLabelAppl");
        newEntity.setPKFields(context);
        newEntity.setNonPKFields(context);
        GenericValue inventoryItemLabel = null;
        try {
            inventoryItemLabel = EntityQuery.use(delegator)
                    .from("InventoryItemLabel")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying InventoryItemLabel: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        newEntity.put("inventoryItemLabelTypeId", inventoryItemLabel.get("inventoryItemLabelTypeId"));
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Update an InventoryItemLabel
     */
    public static Map<String, Object> updateInventoryItemLabelAppl(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("InventoryItemLabelAppl")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying InventoryItemLabelAppl: " + e.getMessage(), MODULE);
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
     * Delete an InventoryItemLabel
     */
    public static Map<String, Object> deleteInventoryItemLabelAppl(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("InventoryItemLabelAppl")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying InventoryItemLabelAppl: " + e.getMessage(), MODULE);
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
     * If product store setOwnerUponIssuance is Y or empty, set the inventory item owner upon issuance.
     */
    public static Map<String, Object> changeOwnerUponIssuance(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<GenericValue> orderRoles = null;
        Map<String, Object> orderRoleAndMap = null;
        Map<String, Object> updateContext = null;
        GenericValue orderHeader = null;
        GenericValue orderRole = null;
        GenericValue productStore = null;
        GenericValue itemIssuance = null;
        try {
            itemIssuance = EntityQuery.use(delegator)
                    .from("ItemIssuance")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ItemIssuance: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue inventoryItem = null;
        try {
            inventoryItem = itemIssuance.getRelatedOne("InventoryItem", false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related one InventoryItem: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(inventoryItem)) {
            if ("SERIALIZED_INV_ITEM".equals(inventoryItem.get("inventoryItemTypeId"))) {
                try {
                    orderHeader = itemIssuance.getRelatedOne("OrderHeader", false);
                } catch (Exception e) {
                    Debug.logError(e, "Error getting related one OrderHeader: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (UtilValidate.isNotEmpty(orderHeader)) {
                    orderRoleAndMap.put("orderId", orderHeader.get("orderId"));
                    orderRoleAndMap.put("roleTypeId", "END_USER_CUSTOMER");
                    try {
                        orderRoles = EntityQuery.use(delegator)
                                .from("OrderRole")
                                .where(orderRoleAndMap)
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying OrderRole: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    orderRole = EntityUtil.getFirst((List<GenericValue>) orderRoles);
                    try {
                        productStore = EntityQuery.use(delegator)
                                .from("ProductStore")
                                .where(UtilMisc.toMap("productStoreId", orderHeader.get("productStoreId")))
                                .queryOne();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying ProductStore: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    Object updateContext_ownerPartyId = null;
                    if ((!(UtilValidate.isEmpty(orderRole)) && (UtilValidate.isEmpty(productStore) || UtilValidate.isEmpty(productStore.get("setOwnerUponIssuance")) || "Y".equals(productStore.get("setOwnerUponIssuance"))))) {
                        updateContext.put("ownerPartyId", orderRole.get("partyId"));
                    }
                }
                updateContext.put("inventoryItemId", inventoryItem.get("inventoryItemId"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("updateInventoryItem", updateContext);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling updateInventoryItem: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }

        return result;
    }


    /**
     * Sets priority of an order for Inventory Reservation, orders with HIGH priority would be served first.
     */
    public static Map<String, Object> setOrderReservationPriority(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue oisgir = null;
        List<GenericValue> oisgirais = null;
        List<GenericValue> oisgirs = null;
        Map<String, Object> reassignInventoryReservationsCtx = new HashMap<>();
        GenericValue orderHeader = null;
        Object orderId = context.get("orderId");
        try {
            orderHeader = EntityQuery.use(delegator)
                    .from("OrderHeader")
                    .where(UtilMisc.toMap("orderId", orderId))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderHeader: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Object priority = context.get("priority");
        if (UtilValidate.isEmpty(priority)) {
            try {
                oisgirs = EntityQuery.use(delegator)
                        .from("OrderItemShipGrpInvRes")
                        .where(UtilMisc.toMap("orderId", orderId))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (oisgirs != null) {
                for (GenericValue oisgir_iter : oisgirs) {
                    oisgir = oisgir_iter;
                    oisgir.put("priority", null);
                    try {
                        delegator.store(oisgir);
                    } catch (Exception e) {
                        Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            }
            orderHeader.put("priority", null);
            try {
                delegator.store(orderHeader);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        } else {
            orderHeader.put("priority", priority);
            try {
                delegator.store(orderHeader);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            try {
                oisgirs = EntityQuery.use(delegator)
                        .from("OrderItemShipGrpInvRes")
                        .where(UtilMisc.toMap("orderId", orderId))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (oisgirs != null) {
                for (GenericValue oisgir_iter : oisgirs) {
                    oisgir = oisgir_iter;
                    oisgir.put("priority", priority);
                    try {
                        delegator.store(oisgir);
                    } catch (Exception e) {
                        Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    oisgir = null;
                }
            }
            try {
                oisgirais = EntityQuery.use(delegator)
                        .from("OrderItemAndShipGrpInvResAndItem")
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying OrderItemAndShipGrpInvResAndItem: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (oisgirais != null) {
                for (GenericValue oisgir_iter : oisgirais) {
                    oisgir = oisgir_iter;
                    reassignInventoryReservationsCtx.put("productId", oisgir.get("productId"));
                    reassignInventoryReservationsCtx.put("facilityId", oisgir.get("facilityId"));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("reassignInventoryReservations", reassignInventoryReservationsCtx);
                        if (ServiceUtil.isError(serviceResult)) {
                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling reassignInventoryReservations: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    reassignInventoryReservationsCtx = new HashMap<String, Object>();
                }
            }
        }

        return result;
    }


    /**
     * Service that updates stock availability of products
     */
    public static Map<String, Object> setLastInventoryCount(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        List<GenericValue> productFacilities = null;
        Object facilityId = null;
        String autoItemFacility = null;
        Object serviceInMap = null;
        Boolean useEntityCache = null;
        Object quantityOnHandTotal = null;
        Object availableToPromiseTotal = null;
        Map<String, Object> inlineResult = null;
        GenericValue inventoryItem = null;
        try {
            inventoryItem = EntityQuery.use(delegator)
                    .from("InventoryItem")
                    .where(UtilMisc.toMap("inventoryItemId", context.get("inventoryItemId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying InventoryItem: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(inventoryItem)) {
            error_list.add("Could not lookup InventoryItem '${parameters.inventoryItemId}'");
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        }
        Object productId = inventoryItem.get("productId");
        result.put("productId", productId);
        GenericValue product = null;
        try {
            product = EntityQuery.use(delegator)
                    .from("Product")
                    .where(UtilMisc.toMap("productId", productId))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Product: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Object isMarketingPkgAuto = (Boolean) GroovyUtil.eval("org.ofbiz.entity.util.EntityTypeUtil.hasParentType(delegator, 'ProductType', 'productTypeId', product.productTypeId, 'parentTypeId', 'MARKETING_PKG_AUTO')", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
        Object inventoryItemDetail = context.get("inventoryItemDetail");
        Object updateSingleFacility = UtilProperties.getMessage("inventory", "inventory.cache.updateSingleFacility", locale);
        if ((Boolean.TRUE.equals(updateSingleFacility) && !(UtilValidate.isEmpty(inventoryItem.get("facilityId"))))) {
            try {
                productFacilities = EntityQuery.use(delegator)
                        .from("ProductFacility")
                        .where(UtilMisc.toMap("productId", inventoryItem.get("productId"), "facilityId", inventoryItem.get("facilityId")))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        } else {
            try {
                productFacilities = EntityQuery.use(delegator)
                        .from("ProductFacility")
                        .where(UtilMisc.toMap("productId", inventoryItem.get("productId")))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        if (UtilValidate.isEmpty(productFacilities)) {
            autoItemFacility = UtilProperties.getMessage("inventory", "inventory.cache.autoProductFacility.item", locale);
            if (Boolean.TRUE.equals(autoItemFacility)) {
                facilityId = inventoryItem.get("facilityId");
                Debug.logWarning("ProductFacility not found for product '" + productId + "' facility '" + facilityId + "' to update lastInventoryCount for inventory item '" + inventoryItem.get("inventoryItemId") + "'; auto-creating", MODULE);
                serviceInMap = new HashMap<String, Object>();
                ((Map<String, Object>) serviceInMap).put("userLogin", userLogin);
                ((Map<String, Object>) serviceInMap).put("productId", productId);
                ((Map<String, Object>) serviceInMap).put("facilityId", facilityId);
                ((Map<String, Object>) serviceInMap).put("lastInventoryCount", BigDecimal.ZERO);
                ((Map<String, Object>) serviceInMap).put("lastInventoryCountQoh", BigDecimal.ZERO);
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createProductFacility", (Map<String, Object>) serviceInMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createProductFacility: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                GenericValue productFacility = null;
                try {
                    productFacility = EntityQuery.use(delegator)
                            .from("ProductFacility")
                            .where(UtilMisc.toMap("productId", productId, "facilityId", facilityId))
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying ProductFacility: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (UtilValidate.isNotEmpty(productFacility)) {
                    productFacilities.add(productFacility);
                } else {
                    error_list.add("Could not lookup ProductFacility for: ${serviceInMap}");
                    if (!error_list.isEmpty()) {
                        return ServiceUtil.returnError(error_list);
                    }
                }
            }
        }
        Object recountThresholdAtp = UtilProperties.getMessage("inventory", "inventory.cache.recountThresholdAtp", locale);
        recountThresholdAtp = recountThresholdAtp;
        Object recountThresholdQoh = UtilProperties.getMessage("inventory", "inventory.cache.recountThresholdQoh", locale);
        recountThresholdQoh = recountThresholdQoh;
        Object recountEvery = UtilProperties.getMessage("inventory", "inventory.cache.recountEvery", locale);
        recountEvery = recountEvery;
        if (productFacilities != null) {
            for (GenericValue productFacilityEntry : productFacilities) {
                useEntityCache = Boolean.TRUE;
                Debug.logVerbose("productId: " + productFacilityEntry.get("productId") + ", facilityId: " + productFacilityEntry.get("facilityId") + ", lastInventoryCount: " + productFacilityEntry.get("lastInventoryCount") + ",  lastInventoryCountQoh: " + productFacilityEntry.get("lastInventoryCountQoh") + ", recountThresholdAtp: " + recountThresholdAtp + ", recountThresholdQoh: " + recountThresholdQoh + ", recountEvery: " + recountEvery + ",  inventoryItemDetail: " + ((Map<String, Object>) inventoryItemDetail).get("inventoryItemId") + ", updatesSinceLastCount: " + productFacilityEntry.get("updatesSinceLastCount") + ", availableToPromiseDiff: " + ((Map<String, Object>) inventoryItemDetail).get("availableToPromiseDiff") + ",  quantityOnHandDiff: " + ((Map<String, Object>) inventoryItemDetail).get("quantityOnHandDiff"), MODULE);
                if ((UtilValidate.isEmpty(productFacilityEntry.get("lastInventoryCount")) || UtilValidate.isEmpty(productFacilityEntry.get("lastInventoryCountQoh")) || UtilValidate.isEmpty(inventoryItemDetail) || !((((Comparable) recountEvery).compareTo(0L) > 0 || ((Comparable) recountThresholdAtp).compareTo(BigDecimal.ZERO) > 0 || ((Comparable) recountThresholdQoh).compareTo(BigDecimal.ZERO) > 0)) || (((Comparable) recountEvery).compareTo(0L) > 0 && !(UtilValidate.isEmpty(productFacilityEntry.get("updatesSinceLastCount"))) && productFacilityEntry.get("updatesSinceLastCount") != null /* TODO: field compare operator greater-equals */) || (((Comparable) recountThresholdAtp).compareTo(BigDecimal.ZERO) > 0 && productFacilityEntry.get("lastInventoryCount") != null /* TODO: field compare operator less-equals */) || (((Comparable) recountThresholdQoh).compareTo(BigDecimal.ZERO) > 0 && productFacilityEntry.get("lastInventoryCountQoh") != null /* TODO: field compare operator less-equals */))) {
                    useEntityCache = Boolean.FALSE;
                }
                serviceInMap = new HashMap<String, Object>();
                ((Map<String, Object>) serviceInMap).put("productId", productFacilityEntry.get("productId"));
                ((Map<String, Object>) serviceInMap).put("facilityId", productFacilityEntry.get("facilityId"));
                ((Map<String, Object>) serviceInMap).put("useEntityCache", useEntityCache);
                if (Boolean.TRUE.equals(isMarketingPkgAuto)) {
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("getMktgPackagesAvailable", (Map<String, Object>) serviceInMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                        }
                        availableToPromiseTotal = serviceResult.get("availableToPromiseTotal");
                        quantityOnHandTotal = serviceResult.get("quantityOnHandTotal");
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling getMktgPackagesAvailable: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                } else {
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("getInventoryAvailableByFacility", (Map<String, Object>) serviceInMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                        }
                        availableToPromiseTotal = serviceResult.get("availableToPromiseTotal");
                        quantityOnHandTotal = serviceResult.get("quantityOnHandTotal");
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling getInventoryAvailableByFacility: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
                serviceInMap = null;
                serviceInMap = new HashMap<String, Object>();
                // set-service-fields from "productFacility" to "serviceInMap" for service "updateProductFacility"
                ((Map<String, Object>) serviceInMap).putAll(UtilMisc.toMap(productFacilityEntry));
                ((Map<String, Object>) serviceInMap).put("lastInventoryCount", availableToPromiseTotal);
                ((Map<String, Object>) serviceInMap).put("lastInventoryCountQoh", quantityOnHandTotal);
                Long serviceInMap_updatesSinceLastCount = null;
                if ((Boolean.TRUE.equals(useEntityCache) && ((Comparable) recountEvery).compareTo(0L) > 0)) {
                    if (UtilValidate.isNotEmpty(productFacilityEntry.get("updatesSinceLastCount"))) {
                        ((Map<String, Object>) serviceInMap).put("updatesSinceLastCount", ((Number) productFacilityEntry.get("updatesSinceLastCount")).longValue() + 1L);
                    } else {
                        ((Map<String, Object>) serviceInMap).put("updatesSinceLastCount", 1L);
                    }
                } else {
                    ((Map<String, Object>) serviceInMap).put("updatesSinceLastCount", 0L);
                }
                inlineResult = setProductFacilityLastInvMode(dctx, context);
                if (ServiceUtil.isError(inlineResult)) {
                    return inlineResult;
                }
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("updateProductFacility", (Map<String, Object>) serviceInMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling updateProductFacility: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                Debug.logInfo("Updated product facility from InventoryItem (useEntityCache: " + useEntityCache + "): " + serviceInMap, MODULE);
            }
        }

        return result;
    }


    /**
     */
    public static Map<String, Object> setProductFacilityLastInvMode(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> serviceInMap = null;
        Timestamp serviceInMap_lastInvStamp = new Timestamp(System.currentTimeMillis());
        serviceInMap.put("lastInvMode", context.get("lastInvMode"));
        Map<String, Object> productFacility = new HashMap<String, Object>();
        Long serviceInMap_lastInvModeCount = null;
        if ((UtilValidate.isEmpty(((Map<String, Object>) productFacility).get("lastInvModeCount")) || !java.util.Objects.equals(((Map<String, Object>) productFacility).get("lastInvMode"), ((Map<String, Object>) serviceInMap).get("lastInvMode")))) {
            serviceInMap.put("lastInvModeCount", 1L);
        } else {
            serviceInMap.put("lastInvModeCount", (Long) ((Map<String, Object>) productFacility).get("lastInvModeCount+1"));
        }

        return result;
    }


    /**
     */
    public static Map<String, Object> setRelatedProductLastInventoryCount(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Object serviceInMap = null;
        List<GenericValue> productAndAssocList = null;
        try {
            productAndAssocList = EntityQuery.use(delegator)
                    .from("ProductAndAssocAll")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ProductAndAssocAll: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(productAndAssocList)) {
            if (productAndAssocList != null) {
                for (GenericValue productAndAssoc : productAndAssocList) {
                    Debug.logInfo("Updating MARKETING_PKG_AUTO product '" + productAndAssoc.get("productId") + "' lastInventoryCount after inventory change to component '" + context.get("productId") + "'", MODULE);
                    serviceInMap = new HashMap<String, Object>();
                    ((Map<String, Object>) serviceInMap).put("productId", productAndAssoc.get("productId"));
                    ((Map<String, Object>) serviceInMap).put("lastInvMode", context.get("lastInvMode"));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("setProductLastInventoryCount", (Map<String, Object>) serviceInMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling setProductLastInventoryCount: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            }
        }

        return result;
    }


    /**
     * Service that updates stock availability of products (SCIPIO)
     */
    public static Map<String, Object> setProductLastInventoryCount(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        List<GenericValue> productFacilities = null;
        Object serviceInMap = null;
        Object quantityOnHandTotal = null;
        Object availableToPromiseTotal = null;
        Map<String, Object> inlineResult = null;
        Object productId = context.get("productId");
        try {
            productFacilities = EntityQuery.use(delegator)
                    .from("ProductFacility")
                    .where(UtilMisc.toMap("productId", context.get("productId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Object autoProdFacility = UtilProperties.getMessage("inventory", "inventory.cache.autoProductFacility.product", locale);
        if (Boolean.TRUE.equals(autoProdFacility)) {
            try {
                Map<String, Object> scriptContext = new HashMap<String, Object>();
                scriptContext.put("delegator", delegator);
                scriptContext.put("dispatcher", dispatcher);
                scriptContext.put("locale", locale);
                scriptContext.put("userLogin", userLogin);
                scriptContext.put("context", context);
                scriptContext.put("parameters", context);
                Object scriptResult = GroovyUtil.eval("itemFacilityIds = from(\"InventoryItem\").where(\"productId\", parameters.productId).distinct().getFieldSet(\"facilityId\");\n                productFacilities = context.productFacilities;\n                productFacilityIds = (productFacilities != null) ? productFacilities.collect{fac->fac.facilityId} : [] as Set;\n                itemFacilityIds.removeAll(productFacilityIds);\n                context.missingFacilityIds = itemFacilityIds;", scriptContext);
            } catch (Exception e) {
                Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
            }
            if (context.get("missingFacilityIds") != null) {
                for (Object facilityId : (List<?>) context.get("missingFacilityIds")) {
                    Debug.logWarning("ProductFacility not found for product '" + productId + "' facility '" + facilityId + "' to update lastInventoryCount for inventory items; auto-creating", MODULE);
                    serviceInMap = new HashMap<String, Object>();
                    ((Map<String, Object>) serviceInMap).put("userLogin", userLogin);
                    ((Map<String, Object>) serviceInMap).put("productId", productId);
                    ((Map<String, Object>) serviceInMap).put("facilityId", facilityId);
                    ((Map<String, Object>) serviceInMap).put("lastInventoryCount", BigDecimal.ZERO);
                    ((Map<String, Object>) serviceInMap).put("lastInventoryCountQoh", BigDecimal.ZERO);
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("createProductFacility", (Map<String, Object>) serviceInMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling createProductFacility: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    GenericValue productFacility = null;
                    try {
                        productFacility = EntityQuery.use(delegator)
                                .from("ProductFacility")
                                .where(UtilMisc.toMap("productId", productId, "facilityId", facilityId))
                                .queryOne();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying ProductFacility: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    if (UtilValidate.isNotEmpty(productFacility)) {
                        productFacilities.add(productFacility);
                    } else {
                        error_list.add("Could not lookup ProductFacility for: ${serviceInMap}");
                        if (!error_list.isEmpty()) {
                            return ServiceUtil.returnError(error_list);
                        }
                    }
                }
            }
        }
        GenericValue product = null;
        try {
            product = EntityQuery.use(delegator)
                    .from("Product")
                    .where(UtilMisc.toMap("productId", productId))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Product: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Object isMarketingPkgAuto = (Boolean) GroovyUtil.eval("org.ofbiz.entity.util.EntityTypeUtil.hasParentType(delegator, 'ProductType', 'productTypeId', product.productTypeId, 'parentTypeId', 'MARKETING_PKG_AUTO')", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
        if (productFacilities != null) {
            for (GenericValue productFacilityEntry : productFacilities) {
                serviceInMap = new HashMap<String, Object>();
                ((Map<String, Object>) serviceInMap).put("productId", productFacilityEntry.get("productId"));
                ((Map<String, Object>) serviceInMap).put("facilityId", productFacilityEntry.get("facilityId"));
                if (Boolean.TRUE.equals(isMarketingPkgAuto)) {
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("getMktgPackagesAvailable", (Map<String, Object>) serviceInMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                        }
                        availableToPromiseTotal = serviceResult.get("availableToPromiseTotal");
                        quantityOnHandTotal = serviceResult.get("quantityOnHandTotal");
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling getMktgPackagesAvailable: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                } else {
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("getInventoryAvailableByFacility", (Map<String, Object>) serviceInMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                        }
                        availableToPromiseTotal = serviceResult.get("availableToPromiseTotal");
                        quantityOnHandTotal = serviceResult.get("quantityOnHandTotal");
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling getInventoryAvailableByFacility: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
                serviceInMap = null;
                serviceInMap = new HashMap<String, Object>();
                // set-service-fields from "productFacility" to "serviceInMap" for service "updateProductFacility"
                ((Map<String, Object>) serviceInMap).putAll(UtilMisc.toMap(productFacilityEntry));
                ((Map<String, Object>) serviceInMap).put("lastInventoryCount", availableToPromiseTotal);
                ((Map<String, Object>) serviceInMap).put("lastInventoryCountQoh", quantityOnHandTotal);
                ((Map<String, Object>) serviceInMap).put("updatesSinceLastCount", 0L);
                inlineResult = setProductFacilityLastInvMode(dctx, context);
                if (ServiceUtil.isError(inlineResult)) {
                    return inlineResult;
                }
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("updateProductFacility", (Map<String, Object>) serviceInMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling updateProductFacility: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }

        return result;
    }


    /**
     * Create or update GeoPoint assigned to facility
     */
    public static Map<String, Object> createUpdateFacilityGeoPoint(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> updateGeoPointMap = null;
        Map<String, Object> createGeoPointMap = null;
        Object geoPointId = null;
        GenericValue facility = null;
        if (UtilValidate.isEmpty(context.get("geoPointId"))) {
            // set-service-fields from "parameters" to "createGeoPointMap" for service "createGeoPoint"
            createGeoPointMap.putAll(UtilMisc.toMap(context));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createGeoPoint", createGeoPointMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
                geoPointId = serviceResult.get("geoPointId");
            } catch (Exception e) {
                Debug.logError(e, "Error calling createGeoPoint: " + e.getMessage(), MODULE);
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
            facility.put("geoPointId", geoPointId);
            try {
                delegator.store(facility);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        } else {
            // set-service-fields from "parameters" to "updateGeoPointMap" for service "updateGeoPoint"
            updateGeoPointMap.putAll(UtilMisc.toMap(context));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updateGeoPoint", updateGeoPointMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling updateGeoPoint: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        return result;
    }

}

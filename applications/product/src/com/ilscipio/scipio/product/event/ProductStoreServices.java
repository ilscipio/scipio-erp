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
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.security.Security;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://product/script/org/ofbiz/product/store/ProductStoreServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class ProductStoreServices {

    private static final String MODULE = ProductStoreServices.class.getName();


    /**
     * Create a Product Store
     */
    public static Map<String, Object> createProductStore(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Security security = dctx.getSecurity();
        List<String> error_list = new LinkedList<>();

        GenericValue storeFacility = null;
        if (!security.hasEntityPermission("CATALOG", "_CREATE", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("ProductUiLabels", "ProductCatalogCreatePermissionError", locale));
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        if ("Y".equals(context.get("oneInventoryFacility"))) {
            if (UtilValidate.isEmpty(context.get("inventoryFacilityId"))) {
                {
                    String errorMsg = UtilProperties.getMessage("ProductUiLabels", "InventoryFacilityIdRequired", locale);
                    error_list.add(errorMsg);
                }
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        }
        if ("Y".equals(context.get("showPricesWithVatTax"))) {
            if (UtilValidate.isEmpty(context.get("vatTaxAuthGeoId"))) {
                {
                    String errorMsg = UtilProperties.getMessage("ProductUiLabels", "ProductVatTaxAuthGeoNotSet", locale);
                    error_list.add(errorMsg);
                }
            }
            if (UtilValidate.isEmpty(context.get("vatTaxAuthPartyId"))) {
                {
                    String errorMsg = UtilProperties.getMessage("ProductUiLabels", "ProductVatTaxAuthPartyNotSet", locale);
                    error_list.add(errorMsg);
                }
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        }
        GenericValue newEntity = delegator.makeValue("ProductStore");
        newEntity.setNonPKFields(context);
        String productStoreId = delegator.getNextSeqId("ProductStore");
        newEntity.put("productStoreId", productStoreId);
        result.put("productStoreId", productStoreId);
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        if (UtilValidate.isNotEmpty(newEntity.get("inventoryFacilityId"))) {
            storeFacility = delegator.makeValue("ProductStoreFacility");
            storeFacility.put("facilityId", newEntity.get("inventoryFacilityId"));
            storeFacility.put("productStoreId", newEntity.get("productStoreId"));
            storeFacility.put("fromDate", nowTimestamp);
            try {
                delegator.create(storeFacility);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        return result;
    }


    /**
     * Update a Product Store
     */
    public static Map<String, Object> updateProductStore(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Security security = dctx.getSecurity();
        List<String> error_list = new LinkedList<>();

        Map<String, Object> lookupPFMap = null;
        List<GenericValue> emptyField = null;
        GenericValue storeFacility = null;
        List<GenericValue> storeFacilities = null;
        if (!security.hasEntityPermission("CATALOG", "_UPDATE", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("ProductUiLabels", "ProductCatalogUpdatePermissionError", locale));
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        if ("Y".equals(context.get("oneInventoryFacility"))) {
            if (UtilValidate.isEmpty(context.get("inventoryFacilityId"))) {
                {
                    String errorMsg = UtilProperties.getMessage("ProductUiLabels", "InventoryFacilityIdRequired", locale);
                    error_list.add(errorMsg);
                }
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        }
        Map<String, Object> lookupPKMap = new HashMap<String, Object>();
        lookupPKMap.put("productStoreId", context.get("productStoreId"));
        GenericValue store = null;
        try {
            store = EntityQuery.use(delegator)
                    .from("ProductStore")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ProductStore: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Object oldFacilityId = store.get("inventoryFacilityId");
        store.setNonPKFields(context);
        if ("Y".equals(store.get("showPricesWithVatTax"))) {
            if (UtilValidate.isEmpty(store.get("vatTaxAuthGeoId"))) {
                {
                    String errorMsg = UtilProperties.getMessage("ProductUiLabels", "ProductVatTaxAuthGeoNotSet", locale);
                    error_list.add(errorMsg);
                }
            }
            if (UtilValidate.isEmpty(store.get("vatTaxAuthPartyId"))) {
                {
                    String errorMsg = UtilProperties.getMessage("ProductUiLabels", "ProductVatTaxAuthPartyNotSet", locale);
                    error_list.add(errorMsg);
                }
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        }
        try {
            delegator.store(store);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        if (!java.util.Objects.equals(store.get("inventoryFacilityId"), oldFacilityId)) {
            if ("Y".equals(store.get("oneInventoryFacility"))) {
                lookupPFMap.put("productStoreId", store.get("productStoreId"));
                try {
                    storeFacilities = EntityQuery.use(delegator)
                            .from("ProductStoreFacility")
                            .where(lookupPFMap)
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying ProductStoreFacility: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                emptyField = EntityUtil.filterByDate(UtilGenerics.cast(storeFacilities));
                if (storeFacilities != null) {
                    for (GenericValue facility : storeFacilities) {
                        facility.put("thruDate", nowTimestamp);
                        try {
                            delegator.store(facility);
                        } catch (Exception e) {
                            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                    }
                }
            }
            storeFacility = delegator.makeValue("ProductStoreFacility");
            storeFacility.put("facilityId", store.get("inventoryFacilityId"));
            storeFacility.put("productStoreId", store.get("productStoreId"));
            storeFacility.put("fromDate", nowTimestamp);
            try {
                delegator.create(storeFacility);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        return result;
    }


    /**
     * Reserve Store Inventory
     */
    public static Map<String, Object> reserveStoreInventory(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue storeFound = null;
        Object facilityId = null;
        List<GenericValue> productStoreFacilities = null;
        Object facilityFound = null;
        Object quantityNotReserved = null;
        Object availableToPromiseTotal = null;
        Map<String, Object> callServiceMap = null;
        Object requireInventory = null;
        GenericValue product = null;
        try {
            product = EntityQuery.use(delegator)
                    .from("Product")
                    .where(context)
                    .cache()
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Product: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue productStore = null;
        try {
            productStore = EntityQuery.use(delegator)
                    .from("ProductStore")
                    .where(context)
                    .cache()
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ProductStore: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue orderHeader = null;
        try {
            orderHeader = EntityQuery.use(delegator)
                    .from("OrderHeader")
                    .where(context)
                    .cache()
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderHeader: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        context.put("priority", orderHeader.get("priority"));
        if (UtilValidate.isEmpty(productStore)) {
            {
                String errorMsg = UtilProperties.getMessage("ProductUiLabels", "ProductProductStoreNotFound", locale);
                error_list.add(errorMsg);
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        }
        if ("N".equals(productStore.get("reserveInventory"))) {
            Debug.logVerbose("ProductStore with id " + productStore.get("productStoreId") + ", is set to NOT reserve inventory, not reserving inventory", MODULE);
            result.put("quantityNotReserved", context.get("quantity"));
            return result;
        }
        context.put("product", product);
        context.put("requireInventory", requireInventory);
        context.put("productStore", productStore);
        isStoreInventoryRequiredInline(dctx, context);
        facilityId = context.get("facilityId");
        GenericValue productStoreFacility = null;
        if (UtilValidate.isEmpty(facilityId)) {
            if ("Y".equals(productStore.get("oneInventoryFacility"))) {
                if (UtilValidate.isEmpty(productStore.get("inventoryFacilityId"))) {
                    {
                        String errorMsg = UtilProperties.getMessage("ProductUiLabels", "ProductProductStoreNoSpecifiedInventoryFacility", locale);
                        error_list.add(errorMsg);
                    }
                    if (!error_list.isEmpty()) {
                        return ServiceUtil.returnError(error_list);
                    }
                }
                // set-service-fields from "parameters" to "callServiceMap" for service "reserveProductInventoryByFacility"
                callServiceMap.putAll(UtilMisc.toMap(context));
                callServiceMap.put("facilityId", productStore.get("inventoryFacilityId"));
                callServiceMap.put("requireInventory", requireInventory);
                callServiceMap.put("reserveOrderEnumId", productStore.get("reserveOrderEnumId"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("reserveProductInventoryByFacility", callServiceMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                    quantityNotReserved = serviceResult.get("quantityNotReserved");
                } catch (Exception e) {
                    Debug.logError(e, "Error calling reserveProductInventoryByFacility: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (java.util.Objects.equals(quantityNotReserved, BigDecimal.ZERO)) {
                    Debug.logInfo("Inventory IS reserved in facility with id [" + productStore.get("inventoryFacilityId") + "] for product id [" + context.get("productId") + "]; desired quantity was " + context.get("quantity"), MODULE);
                } else {
                    Debug.logInfo("There is insufficient inventory available in facility with id [" + productStore.get("inventoryFacilityId") + "] for product id [" + context.get("productId") + "]; desired quantity is " + context.get("quantity") + ", amount could not reserve is " + quantityNotReserved, MODULE);
                }
            } else {
                try {
                    productStoreFacilities = EntityQuery.use(delegator)
                            .from("ProductStoreFacility")
                            .where(UtilMisc.toMap("productStoreId", productStore.get("productStoreId")))
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (productStoreFacilities != null) {
                    for (GenericValue productStoreFacilityEntry : productStoreFacilities) {
                        if (UtilValidate.isEmpty(storeFound)) {
                            callServiceMap.put("productId", context.get("productId"));
                            callServiceMap.put("facilityId", productStoreFacilityEntry.get("facilityId"));
                            Debug.logInfo("ProductStoreService:In productStoreFacilities loop: [" + context.get("facilityId") + "]", MODULE);
                            try {
                                Map<String, Object> serviceResult = dispatcher.runSync("getInventoryAvailableByFacility", callServiceMap);
                                if (ServiceUtil.isError(serviceResult)) {
                                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                                }
                                availableToPromiseTotal = serviceResult.get("availableToPromiseTotal");
                            } catch (Exception e) {
                                Debug.logError(e, "Error calling getInventoryAvailableByFacility: " + e.getMessage(), MODULE);
                                return ServiceUtil.returnError(e.getMessage());
                            }
                            callServiceMap = new HashMap<String, Object>();
                            if (availableToPromiseTotal != null /* TODO: field compare operator greater-equals */) {
                                storeFound = productStoreFacilityEntry;
                            }
                            availableToPromiseTotal = null;
                        }
                    }
                }
                if (UtilValidate.isEmpty(storeFound)) {
                    storeFound = EntityUtil.getFirst((List<GenericValue>) productStoreFacilities);
                }
                facilityId = storeFound.get("facilityId");
                // set-service-fields from "parameters" to "callServiceMap" for service "reserveProductInventoryByFacility"
                callServiceMap.putAll(UtilMisc.toMap(context));
                callServiceMap.put("facilityId", facilityId);
                callServiceMap.put("requireInventory", requireInventory);
                callServiceMap.put("reserveOrderEnumId", productStore.get("reserveOrderEnumId"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("reserveProductInventoryByFacility", callServiceMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                    quantityNotReserved = serviceResult.get("quantityNotReserved");
                } catch (Exception e) {
                    Debug.logError(e, "Error calling reserveProductInventoryByFacility: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                Debug.logInfo("Inventory IS reserved in facility with id [" + storeFound.get("facilityId") + "] for product id [" + context.get("productId") + "]; desired quantity was " + context.get("quantity"), MODULE);
            }
        } else {
            try {
                productStoreFacilities = EntityQuery.use(delegator)
                        .from("ProductStoreFacility")
                        .where(UtilMisc.toMap("productStoreId", productStore.get("productStoreId"), "facilityId", facilityId))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (productStoreFacilities != null) {
                for (GenericValue productStoreFacilityEntry : productStoreFacilities) {
                    facilityFound = productStoreFacilityEntry;
                    Debug.logInfo("ProductStoreService:Facility Found : [" + facilityFound + "]", MODULE);
                }
            }
            if (UtilValidate.isEmpty(facilityFound)) {
                {
                    String errorMsg = UtilProperties.getMessage("ProductUiLabels", "FacilityNoAssociatedWithProcuctStore", locale);
                    error_list.add(errorMsg);
                }
                if (!error_list.isEmpty()) {
                    return ServiceUtil.returnError(error_list);
                }
            }
            // set-service-fields from "parameters" to "callServiceMap" for service "reserveProductInventoryByFacility"
            callServiceMap.putAll(UtilMisc.toMap(context));
            callServiceMap.put("facilityId", facilityId);
            callServiceMap.put("requireInventory", requireInventory);
            callServiceMap.put("reserveOrderEnumId", productStore.get("reserveOrderEnumId"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("reserveProductInventoryByFacility", callServiceMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
                quantityNotReserved = serviceResult.get("quantityNotReserved");
            } catch (Exception e) {
                Debug.logError(e, "Error calling reserveProductInventoryByFacility: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (java.util.Objects.equals(quantityNotReserved, BigDecimal.ZERO)) {
                Debug.logInfo("Inventory IS reserved in facility with id [" + facilityId + "] for product id [" + context.get("productId") + "]; desired quantity was " + context.get("quantity"), MODULE);
            } else {
                Debug.logInfo("There is insufficient inventory available in facility with id [" + facilityId + "] for product id [" + context.get("productId") + "]; desired quantity is " + context.get("quantity") + ", amount could not reserve is " + quantityNotReserved, MODULE);
            }
        }
        result.put("quantityNotReserved", quantityNotReserved);

        return result;
    }


    /**
     * Is Store Inventory Required
     */
    public static Map<String, Object> isStoreInventoryRequired(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue productStore = null;
        GenericValue product = null;
        Object requireInventory = null;
        if (UtilValidate.isEmpty(context.get("productStore"))) {
            try {
                productStore = EntityQuery.use(delegator)
                        .from("ProductStore")
                        .where(context)
                        .cache()
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ProductStore: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        } else {
            productStore = (GenericValue) context.get("productStore");
        }
        if (UtilValidate.isEmpty(context.get("product"))) {
            try {
                product = EntityQuery.use(delegator)
                        .from("Product")
                        .where(context)
                        .cache()
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying Product: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        } else {
            product = (GenericValue) context.get("product");
        }
        context.put("product", product);
        context.put("requireInventory", requireInventory);
        context.put("productStore", productStore);
        isStoreInventoryRequiredInline(dctx, context);
        result.put("requireInventory", requireInventory);

        return result;
    }


    /**
     * Is Store Inventory Required
     */
    public static Map<String, Object> isStoreInventoryRequiredInline(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Object requireInventory = null;
        requireInventory = ((Map<String, Object>) context.get("product")).get("requireInventory");
        if (UtilValidate.isEmpty(requireInventory)) {
            requireInventory = ((Map<String, Object>) context.get("productStore")).get("requireInventory");
        }
        if (UtilValidate.isEmpty(requireInventory)) {
            requireInventory = "N";
        }

        return result;
    }


    /**
     * Is Store Inventory Available
     */
    public static Map<String, Object> isStoreInventoryAvailable(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue productStore = null;
        GenericValue product = null;
        Object available = null;
        List<GenericValue> productStoreFacilities = null;
        Boolean isMarketingPkg = null;
        Object availableToPromiseTotal = null;
        Map<String, Object> callServiceMap = null;
        if (UtilValidate.isEmpty(context.get("productStore"))) {
            try {
                productStore = EntityQuery.use(delegator)
                        .from("ProductStore")
                        .where(context)
                        .cache()
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ProductStore: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        } else {
            productStore = (GenericValue) context.get("productStore");
        }
        if (UtilValidate.isEmpty(context.get("product"))) {
            try {
                product = EntityQuery.use(delegator)
                        .from("Product")
                        .where(context)
                        .cache()
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying Product: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        } else {
            product = (GenericValue) context.get("product");
        }
        if (("SERVICE".equals(product.get("productTypeId")) || "DIGITAL_GOOD".equals(product.get("productTypeId")))) {
            Debug.logVerbose("Product with id " + product.get("productId") + ", is of type " + product.get("productTypeId") + ", returning true for inventory available check", MODULE);
            available = "Y";
            result.put("available", available);
            return result;
        }
        if ("N".equals(productStore.get("checkInventory"))) {
            Debug.logVerbose("ProductStore with id " + productStore.get("productStoreId") + ", is set to NOT check inventory, returning true for inventory available check", MODULE);
            available = "Y";
            result.put("available", available);
            return result;
        }
        GenericValue productStoreFacility = null;
        if ("Y".equals(productStore.get("oneInventoryFacility"))) {
            if (UtilValidate.isEmpty(productStore.get("inventoryFacilityId"))) {
                {
                    String errorMsg = UtilProperties.getMessage("ProductUiLabels", "ProductProductStoreNotCheckAvailability", locale);
                    error_list.add(errorMsg);
                }
                if (!error_list.isEmpty()) {
                    return ServiceUtil.returnError(error_list);
                }
            }
            callServiceMap.put("productId", context.get("productId"));
            callServiceMap.put("facilityId", productStore.get("inventoryFacilityId"));
            callServiceMap.put("useEntityCache", context.get("useEntityCache"));
            callServiceMap.put("useInventoryCache", context.get("useInventoryCache"));
            isMarketingPkg = (Boolean) GroovyUtil.eval("org.ofbiz.entity.util.EntityTypeUtil.hasParentType(delegator, 'ProductType', 'productTypeId', product.productTypeId, 'parentTypeId', 'MARKETING_PKG')", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
            if (Boolean.TRUE.equals(isMarketingPkg)) {
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("getMktgPackagesAvailable", callServiceMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                    availableToPromiseTotal = serviceResult.get("availableToPromiseTotal");
                } catch (Exception e) {
                    Debug.logError(e, "Error calling getMktgPackagesAvailable: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            } else {
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("getInventoryAvailableByFacility", callServiceMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                    availableToPromiseTotal = serviceResult.get("availableToPromiseTotal");
                } catch (Exception e) {
                    Debug.logError(e, "Error calling getInventoryAvailableByFacility: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
            if (availableToPromiseTotal != null /* TODO: field compare operator greater-equals */) {
                available = "Y";
                Debug.logInfo("Inventory IS available in facility with id " + productStore.get("inventoryFacilityId") + " for product id " + context.get("productId") + "; desired quantity is " + context.get("quantity") + ", available quantity is " + availableToPromiseTotal, MODULE);
            } else {
                available = "N";
                Debug.logInfo("Returning false because there is insufficient inventory available in facility with id " + productStore.get("inventoryFacilityId") + " for product id " + context.get("productId") + "; desired quantity is " + context.get("quantity") + ", available quantity is " + availableToPromiseTotal, MODULE);
            }
        } else {
            try {
                productStoreFacilities = EntityQuery.use(delegator)
                        .from("ProductStoreFacility")
                        .where(UtilMisc.toMap("productStoreId", productStore.get("productStoreId")))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            available = "N";
            if (productStoreFacilities != null) {
                for (GenericValue productStoreFacilityEntry : productStoreFacilities) {
                    if ("N".equals(available)) {
                        callServiceMap.put("productId", context.get("productId"));
                        callServiceMap.put("facilityId", productStoreFacilityEntry.get("facilityId"));
                        callServiceMap.put("useEntityCache", context.get("useEntityCache"));
                        callServiceMap.put("useInventoryCache", context.get("useInventoryCache"));
                        isMarketingPkg = (Boolean) GroovyUtil.eval("org.ofbiz.entity.util.EntityTypeUtil.hasParentType(delegator, 'ProductType', 'productTypeId', product.productTypeId, 'parentTypeId', 'MARKETING_PKG')", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
                        if (Boolean.TRUE.equals(isMarketingPkg)) {
                            try {
                                Map<String, Object> serviceResult = dispatcher.runSync("getMktgPackagesAvailable", callServiceMap);
                                if (ServiceUtil.isError(serviceResult)) {
                                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                                }
                                availableToPromiseTotal = serviceResult.get("availableToPromiseTotal");
                            } catch (Exception e) {
                                Debug.logError(e, "Error calling getMktgPackagesAvailable: " + e.getMessage(), MODULE);
                                return ServiceUtil.returnError(e.getMessage());
                            }
                        } else {
                            try {
                                Map<String, Object> serviceResult = dispatcher.runSync("getInventoryAvailableByFacility", callServiceMap);
                                if (ServiceUtil.isError(serviceResult)) {
                                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                                }
                                availableToPromiseTotal = serviceResult.get("availableToPromiseTotal");
                            } catch (Exception e) {
                                Debug.logError(e, "Error calling getInventoryAvailableByFacility: " + e.getMessage(), MODULE);
                                return ServiceUtil.returnError(e.getMessage());
                            }
                        }
                        callServiceMap = new HashMap<String, Object>();
                        if (availableToPromiseTotal != null /* TODO: field compare operator greater-equals */) {
                            available = "Y";
                            Debug.logInfo("Inventory IS available in facility with id " + productStoreFacilityEntry.get("facilityId") + " for product id " + context.get("productId") + "; desired quantity is " + context.get("quantity") + ", available quantity is " + availableToPromiseTotal, MODULE);
                        }
                        availableToPromiseTotal = null;
                    }
                }
            }
        }
        result.put("available", available);

        return result;
    }


    /**
     * Is Store Inventory Available or Not Required
     */
    public static Map<String, Object> isStoreInventoryAvailableOrNotRequired(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue productStore = null;
        GenericValue product = null;
        Object availableOrNotRequired = null;
        Map<String, Object> callServiceMap = null;
        Object requireInventory = null;
        if (UtilValidate.isEmpty(context.get("productStore"))) {
            try {
                productStore = EntityQuery.use(delegator)
                        .from("ProductStore")
                        .where(context)
                        .cache()
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ProductStore: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        } else {
            productStore = (GenericValue) context.get("productStore");
        }
        if (UtilValidate.isEmpty(context.get("product"))) {
            try {
                product = EntityQuery.use(delegator)
                        .from("Product")
                        .where(context)
                        .cache()
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying Product: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        } else {
            product = (GenericValue) context.get("product");
        }
        context.put("product", product);
        context.put("requireInventory", requireInventory);
        context.put("productStore", productStore);
        isStoreInventoryRequiredInline(dctx, context);
        if (!"Y".equals(requireInventory)) {
            availableOrNotRequired = "Y";
            result.put("availableOrNotRequired", availableOrNotRequired);
        } else {
            // set-service-fields from "parameters" to "callServiceMap" for service "isStoreInventoryAvailable"
            callServiceMap.putAll(UtilMisc.toMap(context));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("isStoreInventoryAvailable", callServiceMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
                result.put("available", serviceResult.get("availableOrNotRequired"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling isStoreInventoryAvailable: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        return result;
    }


    /**
     * Check ProductStore Related Permission
     */
    public static Map<String, Object> checkProductStoreRelatedPermission(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Security security = dctx.getSecurity();
        List<String> error_list = new LinkedList<>();

        String callingMethodName = null;
        Object checkAction = null;
        Object productStoreIdName = null;
        Object productStoreIdToCheck = null;
        List<GenericValue> emptyField = null;
        List<GenericValue> roleStores = null;
        Object checkActionLabel = null;
        Boolean hasPermission = null;
        Object resourceDescription = null;
        if (UtilValidate.isEmpty(callingMethodName)) {
            callingMethodName = UtilProperties.getMessage("CommonUiLabels", "CommonPermissionThisOperation", locale);
        }
        if (UtilValidate.isEmpty(checkAction)) {
            checkAction = "UPDATE";
        }
        if (UtilValidate.isEmpty(productStoreIdName)) {
            productStoreIdName = "productStoreId";
        }
        if (UtilValidate.isEmpty(productStoreIdToCheck)) {
            productStoreIdToCheck = ((Map<String, Object>) context).get((String) productStoreIdName);
        }
        if (!(security.hasEntityPermission("CATALOG", "_${checkAction}", userLogin))) {
            try {
                roleStores = EntityQuery.use(delegator)
                        .from("ProductStoreRole")
                        .where(UtilMisc.toMap("productStoreId", productStoreIdToCheck, "partyId", userLogin.get("partyId"), "roleTypeId", "LTD_ADMIN"))
                        .filterByDate()
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            emptyField = EntityUtil.filterByDate(UtilGenerics.cast(roleStores));
        }
        Debug.logInfo("Checking store permission, roleStores=" + roleStores, MODULE);
        if (!((security.hasEntityPermission("CATALOG", "_${checkAction}", userLogin) || (security.hasEntityPermission("CATALOG_ROLE", "_${checkAction}", userLogin) && !(UtilValidate.isEmpty(roleStores)))))) {
            Debug.logVerbose("Permission check failed, user does not have permission", MODULE);
            checkActionLabel = "" + GroovyUtil.eval("'ProductCatalog' + checkAction.charAt(0) + checkAction.substring(1).toLowerCase() + 'PermissionError'", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
            resourceDescription = callingMethodName;
            {
                String errorMsg = UtilProperties.getMessage("ProductUiLabels", "${checkActionLabel}", locale);
                error_list.add(errorMsg);
            }
            hasPermission = Boolean.FALSE;
        }

        return result;
    }


    /**
     * Main permission logic
     */
    public static Map<String, Object> productStoreGenericPermission(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Boolean hasPermission = null;
        String failMessage = null;
        Object checkAction = null;
        List<GenericValue> emptyField = null;
        String callingMethodName = null;
        List<GenericValue> roleStores = null;
        Object checkActionLabel = null;
        Object productStoreIdName = null;
        Object resourceDescription = null;
        Object productStoreIdToCheck = null;
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
        Map<String, Object> inlineResult = checkProductStoreRelatedPermission(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (UtilValidate.isEmpty(error_list)) {
            hasPermission = Boolean.TRUE;
            result.put("hasPermission", hasPermission);
        } else {
            failMessage = UtilProperties.getMessage("ProductUiLabels", "ProductPermissionError", locale);
            hasPermission = Boolean.FALSE;
            result.put("hasPermission", hasPermission);
            result.put("failMessage", failMessage);
        }

        return result;
    }


    /**
     * When product store group hierarchy has been operate, synchronize primaryParentGroupId with ProductStoreGroupRollup
     */
    public static Map<String, Object> checkProductStoreGroupRollup(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> productStoreGroupMap = null;
        List<GenericValue> productStoreGroupRollups = null;
        Map<String, Object> productStoreGroupRollupMap = null;
        GenericValue productStoreGroup = null;
        GenericValue productStoreGroupRollup = null;
        try {
            productStoreGroup = EntityQuery.use(delegator)
                    .from("ProductStoreGroup")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ProductStoreGroup: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(context.get("primaryParentGroupId"))) {
            try {
                productStoreGroupRollup = EntityQuery.use(delegator)
                        .from("ProductStoreGroupRollup")
                        .where(context)
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ProductStoreGroupRollup: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (UtilValidate.isNotEmpty(productStoreGroupRollup)) {
                productStoreGroup.put("primaryParentGroupId", null);
                // set-service-fields from "productStoreGroup" to "productStoreGroupMap" for service "updateProductStoreGroup"
                productStoreGroupMap.putAll(UtilMisc.toMap(productStoreGroup));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("updateProductStoreGroup", productStoreGroupMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling updateProductStoreGroup: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        } else {
            try {
                productStoreGroupRollups = EntityQuery.use(delegator)
                        .from("ProductStoreGroupRollup")
                        .where(UtilMisc.toMap("productStoreGroupId", productStoreGroup.get("productStoreGroupId"), "parentGroupId", context.get("primaryParentGroupId")))
                        .filterByDate()
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (UtilValidate.isEmpty(productStoreGroupRollups)) {
                productStoreGroupRollupMap.put("productStoreGroupId", null);
                productStoreGroupRollupMap.put("parentGroupId", null);
                productStoreGroupRollupMap.put("fromDate", null);
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createProductStoreGroupRollup", productStoreGroupRollupMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createProductStoreGroupRollup: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }

        return result;
    }


    /**
     * Ensure Product Store Store
     */
    public static Map<String, Object> ensureProductStoreRole(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Map<String, Object> createCtx = new HashMap<>();
        GenericValue psr = null;
        List<GenericValue> psrList = null;
        try {
            psrList = EntityQuery.use(delegator)
                    .from("ProductStoreRole")
                    .where(UtilMisc.toMap("partyId", context.get("partyId"), "productStoreId", context.get("productStoreId"), "roleTypeId", context.get("roleTypeId")))
                    .filterByDate()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(psrList)) {
            psr = EntityUtil.getFirst((List<GenericValue>) psrList);
            result.put("fromDate", psr.get("fromDate"));
            if (Boolean.TRUE.equals(context.get("updateOptFields"))) {
                psr.setNonPKFields(context);
                try {
                    delegator.store(psr);
                } catch (Exception e) {
                    Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (!error_list.isEmpty()) {
                    return ServiceUtil.returnError(error_list);
                }
            }
        } else {
            // set-service-fields from "parameters" to "createCtx" for service "createProductStoreRole"
            createCtx.putAll(UtilMisc.toMap(context));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createProductStoreRole", createCtx);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
                result.put("fromDate", serviceResult.get("fromDate"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling createProductStoreRole: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        return result;
    }

}

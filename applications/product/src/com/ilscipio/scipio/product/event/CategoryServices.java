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
import java.sql.Date;
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
 * <p>Generated from: component://product/script/org/ofbiz/product/category/CategoryServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class CategoryServices {

    private static final String MODULE = CategoryServices.class.getName();


    /**
     * Create an ProductCategory
     */
    public static Map<String, Object> createProductCategory(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Security security = dctx.getSecurity();
        List<String> error_list = new LinkedList<>();

        GenericValue newEntity = null;
        List<GenericValue> productCategoryRoles = null;
        GenericValue newLimitRollup = null;
        if (!security.hasEntityPermission("CATALOG", "_CREATE", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("ProductUiLabels", "ProductCatalogCreatePermissionError", locale));
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        newEntity = delegator.makeValue("ProductCategory");
        newEntity.setNonPKFields(context);
        if (UtilValidate.isEmpty(context.get("productCategoryId"))) {
            ((GenericValue) newEntity).put("productCategoryId", delegator.getNextSeqId("ProductCategory"));
        } else {
            newEntity.put("productCategoryId", context.get("productCategoryId"));
            if (newEntity.get("productCategoryId") == null || ((String) newEntity.get("productCategoryId")).trim().isEmpty()) {
                error_list.add("Invalid ID for field newEntity.productCategoryId");
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        }
        result.put("productCategoryId", newEntity.get("productCategoryId"));
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (security.hasEntityPermission("CATALOG_ROLE", "_CREATE", userLogin)) {
            try {
                productCategoryRoles = EntityQuery.use(delegator)
                        .from("ProductCategoryRole")
                        .where(UtilMisc.toMap("partyId", userLogin.get("partyId"), "roleTypeId", "LTD_ADMIN"))
                        .filterByDate()
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (productCategoryRoles != null) {
                for (GenericValue productCategoryRole : productCategoryRoles) {
                    newLimitRollup = delegator.makeValue("ProductCategoryRollup");
                    newLimitRollup.put("productCategoryId", newEntity.get("productCategoryId"));
                    newLimitRollup.put("parentProductCategoryId", productCategoryRole.get("productCategoryId"));
                    newLimitRollup.put("fromDate", nowTimestamp);
                    try {
                        delegator.create(newLimitRollup);
                    } catch (Exception e) {
                        Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            }
        }

        return result;
    }


    /**
     * Update an ProductCategory
     */
    public static Map<String, Object> updateProductCategory(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Object checkAction = null;
        List<GenericValue> emptyField = null;
        String callingMethodName = null;
        Object productCategoryIdName = null;
        Boolean hasPermission = null;
        List<GenericValue> roleCategories = null;
        Object productCategoryIdToCheck = null;
        callingMethodName = "updateProductCategory";
        checkAction = "UPDATE";
        Map<String, Object> inlineResult = checkCategoryRelatedPermission(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ProductCategory")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ProductCategory: " + e.getMessage(), MODULE);
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
     * Delete a ProductCategory (if empty)
     */
    public static Map<String, Object> deleteProductCategory(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Object checkAction = null;
        List<GenericValue> emptyField = null;
        String callingMethodName = null;
        Object productCategoryIdName = null;
        Boolean hasPermission = null;
        List<GenericValue> roleCategories = null;
        Object productCategoryIdToCheck = null;
        callingMethodName = "deleteProductCategory";
        checkAction = "DELETE";
        Map<String, Object> inlineResult = checkCategoryRelatedPermission(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ProductCategory")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ProductCategory: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            delegator.removeValue(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }

        return result;
    }


    /**
     * Add Product to Category
     */
    public static Map<String, Object> addProductToCategory(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue newEntity = delegator.makeValue("ProductCategoryMember");
        newEntity.setPKFields(context);
        newEntity.setNonPKFields(context);
        if (UtilValidate.isEmpty(newEntity.get("fromDate"))) {
            Timestamp newEntity_fromDate = new Timestamp(System.currentTimeMillis());
        }
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        result.put("productId", newEntity.get("productId"));
        result.put("productCategoryId", newEntity.get("productCategoryId"));
        result.put("fromDate", newEntity.get("fromDate"));

        return result;
    }


    /**
     * Add Product to Multiple Categories
     */
    public static Map<String, Object> addProductToCategories(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Object checkAction = null;
        GenericValue newEntity = null;
        Object callingMethodName = null;
        Object productCategoryIdToCheck = null;
        Map<String, Object> inlineResult = null;
        List<GenericValue> emptyField = null;
        Object productCategoryIdName = null;
        Boolean hasPermission = null;
        List<GenericValue> roleCategories = null;
        if (UtilValidate.isEmpty(context.get("fromDate"))) {
            Timestamp parameters_fromDate = new Timestamp(System.currentTimeMillis());
        }
        if (context.get("categories") instanceof List) {
            if (context.get("categories") != null) {
                for (GenericValue category : (List<GenericValue>) context.get("categories")) {
                    newEntity = delegator.makeValue("ProductCategoryMember");
                    newEntity.put("productCategoryId", category);
                    newEntity.setPKFields(context);
                    newEntity.setNonPKFields(context);
                    try {
                        delegator.create(newEntity);
                    } catch (Exception e) {
                        Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            }
        } else {
            productCategoryIdToCheck = context.get("categories");
            callingMethodName = "addProductToCategories";
            checkAction = "CREATE";
            inlineResult = checkCategoryRelatedPermission(dctx, context);
            if (ServiceUtil.isError(inlineResult)) {
                return inlineResult;
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
            newEntity = delegator.makeValue("ProductCategoryMember");
            newEntity.put("productCategoryId", context.get("categories"));
            newEntity.setPKFields(context);
            newEntity.setNonPKFields(context);
            try {
                delegator.create(newEntity);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        result.put("fromDate", context.get("fromDate"));

        return result;
    }


    /**
     * Update Product to Category Application
     */
    public static Map<String, Object> updateProductToCategory(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookupPKMap = delegator.makeValue("ProductCategoryMember");
        lookupPKMap.setPKFields(context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ProductCategoryMember")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ProductCategoryMember: " + e.getMessage(), MODULE);
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
     * Remove Product From Category
     */
    public static Map<String, Object> removeProductFromCategory(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

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
        if (java.util.Objects.equals(product.get("primaryProductCategoryId"), context.get("productCategoryId"))) {
            product.remove("primaryProductCategoryId");
            try {
                delegator.store(product);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        GenericValue lookupPKMap = delegator.makeValue("ProductCategoryMember");
        lookupPKMap.setPKFields(context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ProductCategoryMember")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ProductCategoryMember: " + e.getMessage(), MODULE);
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
     * Add Party to Category
     */
    public static Map<String, Object> addPartyToCategory(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Object checkAction = null;
        List<GenericValue> emptyField = null;
        String callingMethodName = null;
        Object productCategoryIdName = null;
        Boolean hasPermission = null;
        List<GenericValue> roleCategories = null;
        Object productCategoryIdToCheck = null;
        callingMethodName = "addPartyToCategory";
        checkAction = "CREATE";
        Map<String, Object> inlineResult = checkCategoryRelatedPermission(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue newEntity = delegator.makeValue("ProductCategoryRole");
        newEntity.setPKFields(context);
        newEntity.setNonPKFields(context);
        if (UtilValidate.isEmpty(newEntity.get("fromDate"))) {
            Timestamp newEntity_fromDate = new Timestamp(System.currentTimeMillis());
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
     * Update Party to Category Application
     */
    public static Map<String, Object> updatePartyToCategory(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Object checkAction = null;
        List<GenericValue> emptyField = null;
        String callingMethodName = null;
        Object productCategoryIdName = null;
        Boolean hasPermission = null;
        List<GenericValue> roleCategories = null;
        Object productCategoryIdToCheck = null;
        callingMethodName = "updatePartyToCategory";
        checkAction = "UPDATE";
        Map<String, Object> inlineResult = checkCategoryRelatedPermission(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue lookupPKMap = delegator.makeValue("ProductCategoryRole");
        lookupPKMap.setPKFields(context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ProductCategoryRole")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ProductCategoryRole: " + e.getMessage(), MODULE);
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
     * Remove Party From Category
     */
    public static Map<String, Object> removePartyFromCategory(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Object checkAction = null;
        List<GenericValue> emptyField = null;
        String callingMethodName = null;
        Object productCategoryIdName = null;
        Boolean hasPermission = null;
        List<GenericValue> roleCategories = null;
        Object productCategoryIdToCheck = null;
        callingMethodName = "removePartyFromCategory";
        checkAction = "DELETE";
        Map<String, Object> inlineResult = checkCategoryRelatedPermission(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue lookupPKMap = delegator.makeValue("ProductCategoryRole");
        lookupPKMap.setPKFields(context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ProductCategoryRole")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ProductCategoryRole: " + e.getMessage(), MODULE);
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
     * Add ProductCategory to Category
     */
    public static Map<String, Object> addProductCategoryToCategory(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Object checkAction = null;
        List<GenericValue> emptyField = null;
        String callingMethodName = null;
        Object productCategoryIdName = null;
        Boolean hasPermission = null;
        List<GenericValue> roleCategories = null;
        Object productCategoryIdToCheck = null;
        callingMethodName = "addProductCategoryToCategory";
        checkAction = "CREATE";
        productCategoryIdName = "parentProductCategoryId";
        Map<String, Object> inlineResult = checkCategoryRelatedPermission(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue newEntity = delegator.makeValue("ProductCategoryRollup");
        newEntity.setPKFields(context);
        newEntity.setNonPKFields(context);
        if (UtilValidate.isEmpty(newEntity.get("fromDate"))) {
            Timestamp newEntity_fromDate = new Timestamp(System.currentTimeMillis());
        }
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        result.put("productCategoryId", newEntity.get("productCategoryId"));
        result.put("parentProductCategoryId", newEntity.get("parentProductCategoryId"));
        result.put("fromDate", newEntity.get("fromDate"));

        return result;
    }


    /**
     * Add ProductCategory to Categories
     */
    public static Map<String, Object> addProductCategoryToCategories(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Object checkAction = null;
        Object callingMethodName = null;
        GenericValue newEntity = null;
        Object productCategoryIdToCheck = null;
        Map<String, Object> inlineResult = null;
        List<GenericValue> emptyField = null;
        Object productCategoryIdName = null;
        Boolean hasPermission = null;
        List<GenericValue> roleCategories = null;
        if (UtilValidate.isEmpty(context.get("fromDate"))) {
            Timestamp parameters_fromDate = new Timestamp(System.currentTimeMillis());
        }
        if (context.get("categories") instanceof List) {
            if (context.get("categories") != null) {
                for (GenericValue category : (List<GenericValue>) context.get("categories")) {
                    callingMethodName = "addProductCategoryToCategories";
                    checkAction = "CREATE";
                    productCategoryIdToCheck = category;
                    inlineResult = checkCategoryRelatedPermission(dctx, context);
                    if (ServiceUtil.isError(inlineResult)) {
                        return inlineResult;
                    }
                    if (!error_list.isEmpty()) {
                        return ServiceUtil.returnError(error_list);
                    }
                    newEntity = delegator.makeValue("ProductCategoryRollup");
                    newEntity.put("parentProductCategoryId", category);
                    newEntity.setPKFields(context);
                    newEntity.setNonPKFields(context);
                    try {
                        delegator.create(newEntity);
                    } catch (Exception e) {
                        Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            }
        } else {
            callingMethodName = "addProductCategoryToCategories";
            checkAction = "CREATE";
            productCategoryIdToCheck = context.get("categories");
            inlineResult = checkCategoryRelatedPermission(dctx, context);
            if (ServiceUtil.isError(inlineResult)) {
                return inlineResult;
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
            newEntity = delegator.makeValue("ProductCategoryRollup");
            newEntity.put("parentProductCategoryId", context.get("categories"));
            newEntity.setPKFields(context);
            newEntity.setNonPKFields(context);
            try {
                delegator.create(newEntity);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        result.put("fromDate", context.get("fromDate"));

        return result;
    }


    /**
     * Update ProductCategory to Category Application
     */
    public static Map<String, Object> updateProductCategoryToCategory(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Object checkAction = null;
        List<GenericValue> emptyField = null;
        String callingMethodName = null;
        Object productCategoryIdName = null;
        Boolean hasPermission = null;
        List<GenericValue> roleCategories = null;
        Object productCategoryIdToCheck = null;
        callingMethodName = "updateProductCategoryToCategory";
        checkAction = "UPDATE";
        productCategoryIdName = "parentProductCategoryId";
        Map<String, Object> inlineResult = checkCategoryRelatedPermission(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue lookupPKMap = delegator.makeValue("ProductCategoryRollup");
        lookupPKMap.setPKFields(context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ProductCategoryRollup")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ProductCategoryRollup: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        lookedUpValue.setNonPKFields(context);
        try {
            delegator.store(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(context.get("originalProductCategoryId"))) {
            result.put("productCategoryId", context.get("originalProductCategoryId"));
        } else {
            result.put("productCategoryId", context.get("productCategoryId"));
        }

        return result;
    }


    /**
     * Remove ProductCategory From Category
     */
    public static Map<String, Object> removeProductCategoryFromCategory(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Object checkAction = null;
        List<GenericValue> emptyField = null;
        String callingMethodName = null;
        Object productCategoryIdName = null;
        Boolean hasPermission = null;
        List<GenericValue> roleCategories = null;
        Object productCategoryIdToCheck = null;
        callingMethodName = "removeProductCategoryFromCategory";
        checkAction = "DELETE";
        productCategoryIdName = "parentProductCategoryId";
        Map<String, Object> inlineResult = checkCategoryRelatedPermission(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue lookupPKMap = delegator.makeValue("ProductCategoryRollup");
        lookupPKMap.setPKFields(context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ProductCategoryRollup")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ProductCategoryRollup: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            delegator.removeValue(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(context.get("originalProductCategoryId"))) {
            result.put("productCategoryId", context.get("originalProductCategoryId"));
        } else {
            result.put("productCategoryId", context.get("productCategoryId"));
        }

        return result;
    }


    /**
     * copy CategoryProduct Members to a CategoryProductTo
     */
    public static Map<String, Object> copyCategoryProductMembers(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        List<GenericValue> emptyField = null;
        List<Object> pcmsToStore = null;
        GenericValue newProductCategoryMember = null;
        List<GenericValue> productCategoryRollups = null;
        Map<String, Object> lookupChildrenMap = null;
        Map<String, Object> callServiceMap = null;
        Object checkAction = null;
        String callingMethodName = null;
        Object productCategoryIdName = null;
        Boolean hasPermission = null;
        List<GenericValue> roleCategories = null;
        Object productCategoryIdToCheck = null;
        callingMethodName = "copyCategoryProductMembers";
        checkAction = "CREATE";
        productCategoryIdName = "productCategoryIdTo";
        Map<String, Object> inlineResult = checkCategoryRelatedPermission(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        List<GenericValue> productCategoryMembers = null;
        try {
            productCategoryMembers = EntityQuery.use(delegator)
                    .from("ProductCategoryMember")
                    .where(UtilMisc.toMap("productCategoryId", context.get("productCategoryId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Object validDate = context.get("validDate");
        if (UtilValidate.isNotEmpty(validDate)) {
            emptyField = EntityUtil.filterByDate(UtilGenerics.cast(productCategoryMembers), (Timestamp) validDate);
        }
        if (productCategoryMembers != null) {
            for (GenericValue productCategoryMember : productCategoryMembers) {
                newProductCategoryMember = GenericValue.create((GenericValue) productCategoryMember);
                newProductCategoryMember.put("productCategoryId", context.get("productCategoryIdTo"));
                pcmsToStore.add(newProductCategoryMember);
            }
        }
        try {
            delegator.storeAll(UtilGenerics.cast(pcmsToStore));
        } catch (Exception e) {
            Debug.logError(e, "Error storing list: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if ("Y".equals(context.get("recurse"))) {
            lookupChildrenMap.put("parentProductCategoryId", context.get("productCategoryId"));
            try {
                productCategoryRollups = EntityQuery.use(delegator)
                        .from("ProductCategoryRollup")
                        .where(lookupChildrenMap)
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ProductCategoryRollup: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (UtilValidate.isNotEmpty(validDate)) {
                emptyField = EntityUtil.filterByDate(UtilGenerics.cast(productCategoryRollups), (Timestamp) validDate);
            }
            if (productCategoryRollups != null) {
                for (GenericValue productCategoryRollup : productCategoryRollups) {
                    callServiceMap.put("productCategoryId", productCategoryRollup.get("productCategoryId"));
                    callServiceMap.put("productCategoryIdTo", context.get("productCategoryIdTo"));
                    callServiceMap.put("validDate", context.get("validDate"));
                    callServiceMap.put("recurse", context.get("recurse"));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("copyCategoryProductMembers", callServiceMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling copyCategoryProductMembers: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            }
        }

        return result;
    }


    /**
     * a service wrapper for copyCategoryEntities
     */
    public static Map<String, Object> duplicateCategoryEntities(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Security security = dctx.getSecurity();
        List<String> error_list = new LinkedList<>();

        List<GenericValue> categoryEntities = null;
        List<GenericValue> emptyField = null;
        GenericValue categoryEntity = null;
        GenericValue newCategoryEntity = null;
        List<Object> entitiesToStore = null;
        Object callingMethodName = "duplicateCategoryEntities";
        if (!security.hasEntityPermission("CATALOG", "_CREATE", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("ProductUiLabels", "ProductCatalogCreatePermissionError", locale));
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        String entityName = (String) context.get("entityName");
        Object productCategoryId = context.get("productCategoryId");
        Object productCategoryIdTo = context.get("productCategoryIdTo");
        Object validDate = context.get("validDate");
        Map<String, Object> inlineResult = copyCategoryEntities(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }

        return result;
    }


    /**
     * copies all entities of entityName with a productCategoryId to a new entity with a productCategoryIdTo,             filtering them by a timestamp passed in to validDate if necessary
     */
    public static Map<String, Object> copyCategoryEntities(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        String entityName = (String) context.get("entityName");
        List<GenericValue> emptyField = null;
        GenericValue newCategoryEntity = null;
        List<Object> entitiesToStore = null;
        List<GenericValue> categoryEntities = null;
        try {
            categoryEntities = EntityQuery.use(delegator)
                    .from(entityName)
                    .where(UtilMisc.toMap("productCategoryId", context.get("productCategoryId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(context.get("validDate"))) {
            emptyField = EntityUtil.filterByDate(UtilGenerics.cast(categoryEntities), (Timestamp) context.get("validDate"));
        }
        if (categoryEntities != null) {
            for (GenericValue categoryEntity : categoryEntities) {
                newCategoryEntity = GenericValue.create((GenericValue) categoryEntity);
                newCategoryEntity.put("productCategoryId", context.get("productCategoryIdTo"));
                entitiesToStore.add(newCategoryEntity);
            }
        }
        try {
            delegator.storeAll(UtilGenerics.cast(entitiesToStore));
        } catch (Exception e) {
            Debug.logError(e, "Error storing list: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Remove ProductCategory From Category
     */
    public static Map<String, Object> expireAllCategoryProductMembers(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Timestamp expireTimestamp = null;
        Object checkAction = null;
        List<GenericValue> emptyField = null;
        String callingMethodName = null;
        Object productCategoryIdName = null;
        Boolean hasPermission = null;
        List<GenericValue> roleCategories = null;
        Object productCategoryIdToCheck = null;
        callingMethodName = "expireAllCategoryProductMembers";
        checkAction = "UPDATE";
        Map<String, Object> inlineResult = checkCategoryRelatedPermission(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        if (UtilValidate.isNotEmpty(context.get("thruDate"))) {
            expireTimestamp = (Timestamp) context.get("thruDate");
        } else {
            expireTimestamp = new Timestamp(System.currentTimeMillis());
        }
        List<GenericValue> productCategoryMembers = null;
        try {
            productCategoryMembers = EntityQuery.use(delegator)
                    .from("ProductCategoryMember")
                    .where(UtilMisc.toMap("productCategoryId", context.get("productCategoryId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (productCategoryMembers != null) {
            for (GenericValue productCategoryMember : productCategoryMembers) {
                productCategoryMember.put("thruDate", expireTimestamp);
                try {
                    delegator.store(productCategoryMember);
                } catch (Exception e) {
                    Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }

        return result;
    }


    /**
     * Remove ProductCategory From Category
     */
    public static Map<String, Object> removeExpiredCategoryProductMembers(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Timestamp expireTimestamp = null;
        Object checkAction = null;
        List<GenericValue> emptyField = null;
        String callingMethodName = null;
        Object productCategoryIdName = null;
        Boolean hasPermission = null;
        List<GenericValue> roleCategories = null;
        Object productCategoryIdToCheck = null;
        callingMethodName = "removeExpiredCategoryProductMembers";
        checkAction = "DELETE";
        Map<String, Object> inlineResult = checkCategoryRelatedPermission(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        if (UtilValidate.isNotEmpty(context.get("validDate"))) {
            expireTimestamp = (Timestamp) context.get("validDate");
        } else {
            expireTimestamp = new Timestamp(System.currentTimeMillis());
        }
        List<GenericValue> productCategoryMembers = null;
        try {
            productCategoryMembers = EntityQuery.use(delegator)
                    .from("ProductCategoryMember")
                    .where(UtilMisc.toMap("productCategoryId", context.get("productCategoryId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (productCategoryMembers != null) {
            for (GenericValue productCategoryMember : productCategoryMembers) {
                if (productCategoryMember.get("thruDate") != null /* TODO: field compare operator less */) {
                    try {
                        delegator.removeValue(productCategoryMember);
                    } catch (Exception e) {
                        Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            }
        }

        return result;
    }


    /**
     * Create a Product in a Category along with special information such as features
     */
    public static Map<String, Object> createProductInCategory(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Map<String, Object> callCreateProductMap = null;
        Map<String, Object> createDefaultPriceMap = null;
        Map<String, Object> createAverageCostMap = null;
        Map<String, Object> createPfaMap = null;
        Object hasSelectableFeatures = null;
        GenericValue newProduct = null;
        Object checkAction = null;
        List<GenericValue> emptyField = null;
        String callingMethodName = null;
        Object productCategoryIdName = null;
        Boolean hasPermission = null;
        List<GenericValue> roleCategories = null;
        Object productCategoryIdToCheck = null;
        callingMethodName = "createProductInCategory";
        checkAction = "CREATE";
        Map<String, Object> inlineResult = checkCategoryRelatedPermission(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        if (UtilValidate.isEmpty(context.get("currencyUomId"))) {
            context.put("currencyUomId", "USD");
        }
        // set-service-fields from "parameters" to "callCreateProductMap" for service "createProduct"
        callCreateProductMap.putAll(UtilMisc.toMap(context));
        if (UtilValidate.isEmpty(((Map<String, Object>) callCreateProductMap).get("productTypeId"))) {
            callCreateProductMap.put("productTypeId", "FINISHED_GOOD");
        }
        Object productId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createProduct", callCreateProductMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            productId = serviceResult.get("productId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createProduct: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        result.put("productId", productId);
        Map<String, Object> callCreateProductCategoryMemberMap = new HashMap<String, Object>();
        callCreateProductCategoryMemberMap.put("productId", productId);
        callCreateProductCategoryMemberMap.put("productCategoryId", context.get("productCategoryId"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("addProductToCategory", callCreateProductCategoryMemberMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling addProductToCategory: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(context.get("defaultPrice"))) {
            createDefaultPriceMap.put("productId", productId);
            createDefaultPriceMap.put("currencyUomId", context.get("currencyUomId"));
            createDefaultPriceMap.put("price", context.get("defaultPrice"));
            createDefaultPriceMap.put("productStoreGroupId", "_NA_");
            createDefaultPriceMap.put("productPriceTypeId", "DEFAULT_PRICE");
            createDefaultPriceMap.put("productPricePurposeId", "PURCHASE");
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createProductPrice", createDefaultPriceMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling createProductPrice: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        if (UtilValidate.isNotEmpty(context.get("averageCost"))) {
            createAverageCostMap.put("productId", productId);
            createAverageCostMap.put("currencyUomId", context.get("currencyUomId"));
            createAverageCostMap.put("price", context.get("averageCost"));
            createAverageCostMap.put("productStoreGroupId", "_NA_");
            createAverageCostMap.put("productPriceTypeId", "AVERAGE_COST");
            createAverageCostMap.put("productPricePurposeId", "PURCHASE");
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createProductPrice", createAverageCostMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling createProductPrice: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        hasSelectableFeatures = "N";
        for (Map.Entry<String, Object> entry : ((Map<String, Object>) context.get("productFeatureIdByType")).entrySet()) {
            String productFeatureTypeId = entry.getKey();
            Object productFeatureId = entry.getValue();
            Debug.logInfo("Applying feature [" + productFeatureId + "] of type [" + productFeatureTypeId + "] to product [" + productId + "]", MODULE);
            createPfaMap.put("productId", productId);
            createPfaMap.put("productFeatureId", productFeatureId);
            if ("Y".equals(context.get("productFeatureSelectableByType[productFeatureTypeId]"))) {
                createPfaMap.put("productFeatureApplTypeId", "SELECTABLE_FEATURE");
                hasSelectableFeatures = "Y";
            } else {
                createPfaMap.put("productFeatureApplTypeId", "STANDARD_FEATURE");
            }
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("applyFeatureToProduct", createPfaMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling applyFeatureToProduct: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            createPfaMap = new HashMap<String, Object>();
        }
        if ("Y".equals(hasSelectableFeatures)) {
            try {
                newProduct = EntityQuery.use(delegator)
                        .from("Product")
                        .where(context)
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying Product: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            newProduct.put("isVirtual", "Y");
            try {
                delegator.store(newProduct);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        return result;
    }


    /**
     * Duplicate a ProductCategory
     */
    public static Map<String, Object> duplicateProductCategory(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Security security = dctx.getSecurity();
        List<String> error_list = new LinkedList<>();

        Object entityName = null;
        Map<String, Object> inlineResult = null;
        GenericValue newTempValue = null;
        List<GenericValue> foundValues = null;
        List<GenericValue> categoryEntities = null;
        List<GenericValue> emptyField = null;
        GenericValue categoryEntity = null;
        GenericValue newCategoryEntity = null;
        List<Object> entitiesToStore = null;
        Object callingMethodName = "duplicateProductCategory";
        if (!security.hasEntityPermission("CATALOG", "_CREATE", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("ProductUiLabels", "ProductCatalogCreatePermissionError", locale));
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue oldCategory = null;
        try {
            oldCategory = EntityQuery.use(delegator)
                    .from("ProductCategory")
                    .where(UtilMisc.toMap("productCategoryId", context.get("oldProductCategoryId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ProductCategory: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue newCategory = GenericValue.create((GenericValue) oldCategory);
        newCategory.put("productCategoryId", context.get("productCategoryId"));
        try {
            delegator.create(newCategory);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Object productCategoryId = context.get("oldProductCategoryId");
        Object productCategoryIdTo = context.get("productCategoryId");
        if (UtilValidate.isNotEmpty(context.get("duplicateMembers"))) {
            entityName = "ProductCategoryMember";
            inlineResult = copyCategoryEntities(dctx, context);
            if (ServiceUtil.isError(inlineResult)) {
                return inlineResult;
            }
        }
        if (UtilValidate.isNotEmpty(context.get("duplicateContent"))) {
            entityName = "ProductCategoryContent";
            inlineResult = copyCategoryEntities(dctx, context);
            if (ServiceUtil.isError(inlineResult)) {
                return inlineResult;
            }
        }
        if (UtilValidate.isNotEmpty(context.get("duplicateRoles"))) {
            entityName = "ProductCategoryRole";
            inlineResult = copyCategoryEntities(dctx, context);
            if (ServiceUtil.isError(inlineResult)) {
                return inlineResult;
            }
        }
        if (UtilValidate.isNotEmpty(context.get("duplicateAttributes"))) {
            entityName = "ProductCategoryAttribute";
            inlineResult = copyCategoryEntities(dctx, context);
            if (ServiceUtil.isError(inlineResult)) {
                return inlineResult;
            }
        }
        if (UtilValidate.isNotEmpty(context.get("duplicateFeatures"))) {
            entityName = "ProductFeatureCategoryAppl";
            inlineResult = copyCategoryEntities(dctx, context);
            if (ServiceUtil.isError(inlineResult)) {
                return inlineResult;
            }
        }
        if (UtilValidate.isNotEmpty(context.get("duplicateCatalogs"))) {
            entityName = "ProdCatalogCategory";
            inlineResult = copyCategoryEntities(dctx, context);
            if (ServiceUtil.isError(inlineResult)) {
                return inlineResult;
            }
        }
        if (UtilValidate.isNotEmpty(context.get("duplicateParentRollup"))) {
            try {
                foundValues = EntityQuery.use(delegator)
                        .from("ProductCategoryRollup")
                        .where(UtilMisc.toMap("productCategoryId", context.get("oldProductCategoryId")))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (foundValues != null) {
                for (GenericValue foundValue : foundValues) {
                    newTempValue = GenericValue.create((GenericValue) foundValue);
                    newTempValue.put("productCategoryId", context.get("productCategoryId"));
                    try {
                        delegator.create(newTempValue);
                    } catch (Exception e) {
                        Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            }
        }
        if (UtilValidate.isNotEmpty(context.get("duplicateChildRollup"))) {
            try {
                foundValues = EntityQuery.use(delegator)
                        .from("ProductCategoryRollup")
                        .where(UtilMisc.toMap("parentProductCategoryId", context.get("oldProductCategoryId")))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (foundValues != null) {
                for (GenericValue foundValueEntry : foundValues) {
                    newTempValue = GenericValue.create((GenericValue) foundValueEntry);
                    newTempValue.put("parentProductCategoryId", context.get("productCategoryId"));
                    try {
                        delegator.create(newTempValue);
                    } catch (Exception e) {
                        Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            }
        }

        return result;
    }


    /**
     * Create an attribute for a product category
     */
    public static Map<String, Object> createProductCategoryAttribute(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Security security = dctx.getSecurity();
        List<String> error_list = new LinkedList<>();

        if (!security.hasEntityPermission("CATALOG", "_CREATE", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("ProductUiLabels", "ProductCatalogCreatePermissionError", locale));
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue newEntity = delegator.makeValue("ProductCategoryAttribute");
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
     * Update an association between two product categories
     */
    public static Map<String, Object> updateProductCategoryAttribute(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Security security = dctx.getSecurity();
        List<String> error_list = new LinkedList<>();

        if (!security.hasEntityPermission("CATALOG", "_UPDATE", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("ProductUiLabels", "ProductCatalogUpdatePermissionError", locale));
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue lookupPKMap = delegator.makeValue("ProductCategoryAttribute");
        lookupPKMap.setPKFields(context);
        GenericValue ProductCategoryAttributeInstance = null;
        try {
            ProductCategoryAttributeInstance = EntityQuery.use(delegator)
                    .from("ProductCategoryAttribute")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ProductCategoryAttribute: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        ProductCategoryAttributeInstance.setNonPKFields(context);
        try {
            delegator.store(ProductCategoryAttributeInstance);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Delete an association between two product categories
     */
    public static Map<String, Object> deleteProductCategoryAttribute(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Security security = dctx.getSecurity();
        List<String> error_list = new LinkedList<>();

        if (!security.hasEntityPermission("CATALOG", "_DELETE", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("ProductUiLabels", "ProductCatalogDeletePermissionError", locale));
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue lookupPKMap = delegator.makeValue("ProductCategoryAttribute");
        lookupPKMap.setPKFields(context);
        GenericValue ProductCategoryAttributeInstance = null;
        try {
            ProductCategoryAttributeInstance = EntityQuery.use(delegator)
                    .from("ProductCategoryAttribute")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ProductCategoryAttribute: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            delegator.removeValue(ProductCategoryAttributeInstance);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * create a ProductCategoryLink
     */
    public static Map<String, Object> createProductCategoryLink(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue newEntity = null;
        newEntity = delegator.makeValue("ProductCategoryLink");
        newEntity.put("productCategoryId", context.get("productCategoryId"));
        if (UtilValidate.isEmpty(context.get("linkSeqId"))) {
            delegator.setNextSubSeqId(newEntity, "linkSeqId", 5, 1);
            Object linkSeqId = newEntity.get("linkSeqId");
            newEntity.put("linkSeqId", linkSeqId);
        }
        newEntity.setPKFields(context);
        if (UtilValidate.isEmpty(newEntity.get("fromDate"))) {
            Timestamp newEntity_fromDate = new Timestamp(System.currentTimeMillis());
        }
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
     * update a ProductCategoryLink
     */
    public static Map<String, Object> updateProductCategoryLink(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ProductCategoryLink")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ProductCategoryLink: " + e.getMessage(), MODULE);
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
     * delete a ProductCategoryLink
     */
    public static Map<String, Object> deleteProductCategoryLink(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ProductCategoryLink")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ProductCategoryLink: " + e.getMessage(), MODULE);
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
     * Check Product Category Related Permission
     */
    public static Map<String, Object> checkCategoryRelatedPermission(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Security security = dctx.getSecurity();
        List<String> error_list = new LinkedList<>();

        String callingMethodName = null;
        Object checkAction = null;
        Object productCategoryIdName = null;
        Object productCategoryIdToCheck = null;
        List<GenericValue> emptyField = null;
        List<GenericValue> roleCategories = null;
        Boolean hasPermission = null;
        if (UtilValidate.isEmpty(callingMethodName)) {
            callingMethodName = UtilProperties.getMessage("CommonUiLabels", "CommonPermissionThisOperation", locale);
        }
        if (UtilValidate.isEmpty(checkAction)) {
            checkAction = "UPDATE";
        }
        if (UtilValidate.isEmpty(productCategoryIdName)) {
            productCategoryIdName = "productCategoryId";
        }
        if (UtilValidate.isEmpty(productCategoryIdToCheck)) {
            productCategoryIdToCheck = ((Map<String, Object>) context).get((String) productCategoryIdName);
        }
        if (!(security.hasEntityPermission("CATALOG", "_${checkAction}", userLogin))) {
            try {
                roleCategories = EntityQuery.use(delegator)
                        .from("ProductCategoryRollupAndRole")
                        .where(UtilMisc.toMap("productCategoryId", productCategoryIdToCheck, "partyId", userLogin.get("partyId"), "roleTypeId", "LTD_ADMIN"))
                        .filterByDate()
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            emptyField = EntityUtil.filterByDate(UtilGenerics.cast(roleCategories));
        }
        Debug.logInfo("Checking category permission, roleCategories=" + roleCategories, MODULE);
        if (!((security.hasEntityPermission("CATALOG", "_${checkAction}", userLogin) || (security.hasEntityPermission("CATALOG_ROLE", "_${checkAction}", userLogin) && !(UtilValidate.isEmpty(roleCategories)))))) {
            Debug.logVerbose("Permission check failed, user does not have permission", MODULE);
            {
                String errorMsg = UtilProperties.getMessage("ProductUiLabels", "ProductCatalogCreatePermissionError", locale);
                error_list.add(errorMsg);
            }
            hasPermission = Boolean.FALSE;
        }

        return result;
    }


    /**
     * Main permission logic
     */
    public static Map<String, Object> productCategoryGenericPermission(DispatchContext dctx, Map<String, Object> context) {
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
        Object productCategoryIdName = null;
        List<GenericValue> roleCategories = null;
        Object productCategoryIdToCheck = null;
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
        Map<String, Object> inlineResult = checkCategoryRelatedPermission(dctx, context);
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
     * Check Product Category Permission With View and Purchase Allow
     */
    public static Map<String, Object> checkCategoryPermissionWithViewPurchaseAllow(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Security security = dctx.getSecurity();
        String resourceDescription = null;
        Boolean hasPermission = null;
        GenericValue prodCatalog = null;
        Object failMessage = null;
        Map<String, Object> productCategoryGenericPermissionMap = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "productCategoryGenericPermissionMap" for service "productCategoryGenericPermission"
        productCategoryGenericPermissionMap.putAll(UtilMisc.toMap(context));
        Map<String, Object> genericResult = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("productCategoryGenericPermission", productCategoryGenericPermissionMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            genericResult = serviceResult;
        } catch (Exception e) {
            Debug.logError(e, "Error calling productCategoryGenericPermission: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (Boolean.FALSE.equals(((Map<String, Object>) genericResult).get("hasPermission"))) {
            result.put("hasPermission", ((Map<String, Object>) genericResult).get("hasPermission"));
            result.put("genericResult.failMessage", ((Map<String, Object>) ((Map<String, Object>) failMessage).get("genericResult")).get("failMessage"));
            return result;
        }
        hasPermission = Boolean.TRUE;
        resourceDescription = (String) context.get("resourceDescription");
        if (UtilValidate.isEmpty(resourceDescription)) {
            resourceDescription = UtilProperties.getMessage("CommonUiLabels", "CommonPermissionThisOperation", locale);
        }
        Object callingMethodName = resourceDescription;
        Object checkAction = context.get("mainAction");
        List<GenericValue> prodCatalogCategoryList = null;
        try {
            prodCatalogCategoryList = EntityQuery.use(delegator)
                    .from("ProdCatalogCategory")
                    .filterByDate()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ProdCatalogCategory: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (prodCatalogCategoryList != null) {
            for (GenericValue prodCatalogCategory : prodCatalogCategoryList) {
                try {
                    prodCatalog = EntityQuery.use(delegator)
                            .from("ProdCatalog")
                            .where(UtilMisc.toMap("prodCatalogId", prodCatalogCategory.get("prodCatalogId")))
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying ProdCatalog: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (("Y".equals(prodCatalog.get("viewAllowPermReqd")) && !(security.hasPermission("CATALOG_VIEW_ALLOW", userLogin)))) {
                    Debug.logVerbose("Permission check failed, user does not have permission", MODULE);
                    failMessage = "Security Error: to run " + callingMethodName + " you must have the CATALOG_VIEW_ALLOW permission.";
                    hasPermission = Boolean.FALSE;
                }
                if (("Y".equals(prodCatalog.get("purchaseAllowPermReqd")) && !(security.hasPermission("CATALOG_PURCHASE_ALLOW", userLogin)))) {
                    Debug.logVerbose("Permission check failed, user does not have permission", MODULE);
                    failMessage = "Security Error: to run " + callingMethodName + " you must have the CATALOG_PURCHASE_ALLOW permission.";
                    hasPermission = Boolean.FALSE;
                }
            }
        }
        result.put("hasPermission", hasPermission);
        result.put("failMessage", failMessage);

        return result;
    }


    /**
     * Set the product options for selected product category, mostly used by getDependentDropdownValues
     */
    public static Map<String, Object> getAssociatedProductsList(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue product = null;
        Object productName = null;
        List<Object> products = null;
        String noOption = null;
        context.put("categoryId", context.get("productCategoryId"));
        Map<String, Object> getProductCategoryMembersMap = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "getProductCategoryMembersMap" for service "getProductCategoryMembers"
        getProductCategoryMembersMap.putAll(UtilMisc.toMap(context));
        Object productsList = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("getProductCategoryMembers", getProductCategoryMembersMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            productsList = serviceResult.get("categoryMembers");
        } catch (Exception e) {
            Debug.logError(e, "Error calling getProductCategoryMembers: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        productsList = EntityUtil.orderBy(UtilGenerics.cast(productsList), UtilMisc.toList("sequenceNum"));
        if (productsList != null) {
            for (Object productMember : (List<?>) productsList) {
                try {
                    product = EntityQuery.use(delegator)
                            .from("Product")
                            .where(UtilMisc.toMap("productId", ((Map<String, Object>) productMember).get("productId")))
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying Product: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                productName = "" + product.get("internalName") + ": " + product.get("productId");
                products.add(productName);
            }
        }
        if (UtilValidate.isEmpty(products)) {
            noOption = UtilProperties.getMessage("ProductUiLabels", "ProductNoProducts", locale);
            products.add(noOption);
        }
        result.put("products", products);

        return result;
    }


    /**
     * Load data of best selling category by week.
     */
    public static Map<String, Object> loadBestSellingCategory(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Object week = null;
        Object year = null;
        Map<String, Object> callRemoveProductMap = null;
        Map<String, Object> callAddProductMap = null;
        GenericValue productStoreCatalog = null;
        Date nowDate = new Date(System.currentTimeMillis());
        week = (Long) GroovyUtil.eval("import java.util.Calendar;             Calendar cal = Calendar.getInstance();             cal.setTime(nowDate);             return cal.get(Calendar.WEEK_OF_YEAR);", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
        year = (Long) GroovyUtil.eval("import java.util.Calendar;             Calendar cal = Calendar.getInstance();             cal.setTime(nowDate);             int aa = cal.get(Calendar.YEAR);             return aa;", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
        if ("1".equals(week)) {
            week = "52";
        } else {
            week = new BigDecimal(week.toString());
            if ("1".equals(week)) {
                year = new BigDecimal(year.toString());
            }
        }
        List<GenericValue> productStoreCatalogs = null;
        try {
            productStoreCatalogs = EntityQuery.use(delegator)
                    .from("ProductStoreCatalog")
                    .where(UtilMisc.toMap("productStoreId", context.get("productStoreId")))
                    .filterByDate()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(productStoreCatalogs)) {
            productStoreCatalog = EntityUtil.getFirst((List<GenericValue>) productStoreCatalogs);
            callRemoveProductMap.put("prodCatalogId", productStoreCatalog.get("prodCatalogId"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("RemoveProductFromBestSellCategory", callRemoveProductMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling RemoveProductFromBestSellCategory: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            callAddProductMap.put("productStoreId", context.get("productStoreId"));
            callAddProductMap.put("prodCatalogId", productStoreCatalog.get("prodCatalogId"));
            callAddProductMap.put("week", week);
            callAddProductMap.put("year", year);
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("AddProductToBestSellCategory", callAddProductMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling AddProductToBestSellCategory: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        return result;
    }


    /**
     * Remove products from best selling category.
     */
    public static Map<String, Object> RemoveProductFromBestSellCategory(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<GenericValue> productCategoryRollups = null;
        List<GenericValue> productCategoryMembers = null;
        List<GenericValue> prodCatalogCategorys = null;
        try {
            prodCatalogCategorys = EntityQuery.use(delegator)
                    .from("ProdCatalogCategory")
                    .where(UtilMisc.toMap("prodCatalogId", context.get("prodCatalogId"), "prodCatalogCategoryTypeId", "PCCT_BEST_SELL"))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (prodCatalogCategorys != null) {
            for (GenericValue prodCatalogCategory : prodCatalogCategorys) {
                try {
                    productCategoryRollups = EntityQuery.use(delegator)
                            .from("ProductCategoryRollup")
                            .where(UtilMisc.toMap("parentProductCategoryId", prodCatalogCategory.get("productCategoryId")))
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (productCategoryRollups != null) {
                    for (GenericValue productCategoryRollup : productCategoryRollups) {
                        try {
                            productCategoryMembers = EntityQuery.use(delegator)
                                    .from("ProductCategoryMember")
                                    .where(UtilMisc.toMap("productCategoryId", productCategoryRollup.get("productCategoryId")))
                                    .queryList();
                        } catch (Exception e) {
                            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                        if (productCategoryMembers != null) {
                            for (GenericValue productCategoryMember : productCategoryMembers) {
                                try {
                                    delegator.removeValue(productCategoryMember);
                                } catch (Exception e) {
                                    Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                                    return ServiceUtil.returnError(e.getMessage());
                                }
                            }
                        }
                    }
                }
            }
        }

        return result;
    }


    /**
     * Add products to best selling category.
     */
    public static Map<String, Object> AddProductToBestSellCategory(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> CategoryChildMap = null;
        List<GenericValue> prodCatalogCategorys = null;
        try {
            prodCatalogCategorys = EntityQuery.use(delegator)
                    .from("ProdCatalogCategory")
                    .where(UtilMisc.toMap("prodCatalogId", context.get("prodCatalogId"), "prodCatalogCategoryTypeId", "PCCT_BEST_SELL"))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue prodCatalogCategory = EntityUtil.getFirst((List<GenericValue>) prodCatalogCategorys);
        List<GenericValue> productCategoryRollupList = null;
        try {
            productCategoryRollupList = EntityQuery.use(delegator)
                    .from("ProductCategoryRollup")
                    .where(UtilMisc.toMap("parentProductCategoryId", prodCatalogCategory.get("productCategoryId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (productCategoryRollupList != null) {
            for (GenericValue productCategoryRollup : productCategoryRollupList) {
                // set-service-fields from "parameters" to "CategoryChildMap" for service "FindCategoryChild"
                CategoryChildMap.putAll(UtilMisc.toMap(context));
                CategoryChildMap.put("productCategoryId", productCategoryRollup.get("productCategoryId"));
                CategoryChildMap.put("primaryProductCategoryId", productCategoryRollup.get("productCategoryId"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("FindCategoryChild", CategoryChildMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling FindCategoryChild: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }

        return result;
    }


    /**
     * Find category child.
     */
    public static Map<String, Object> FindCategoryChild(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> AddProductMap = null;
        Map<String, Object> CategoryChildMap = null;
        List<GenericValue> productCategoryRollupList = null;
        try {
            productCategoryRollupList = EntityQuery.use(delegator)
                    .from("ProductCategoryRollup")
                    .where(UtilMisc.toMap("parentProductCategoryId", context.get("productCategoryId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue productCategoryRollup = null;
        if (UtilValidate.isEmpty(productCategoryRollupList)) {
            // set-service-fields from "parameters" to "AddProductMap" for service "FindBestSellingProduct"
            AddProductMap.putAll(UtilMisc.toMap(context));
            AddProductMap.put("productCategoryId", context.get("productCategoryId"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("FindBestSellingProduct", AddProductMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling FindBestSellingProduct: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        } else {
            if (productCategoryRollupList != null) {
                for (GenericValue productCategoryRollupEntry : productCategoryRollupList) {
                    // set-service-fields from "parameters" to "CategoryChildMap" for service "FindCategoryChild"
                    CategoryChildMap.putAll(UtilMisc.toMap(context));
                    CategoryChildMap.put("productCategoryId", productCategoryRollupEntry.get("productCategoryId"));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("FindCategoryChild", CategoryChildMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling FindCategoryChild: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            }
        }

        return result;
    }


    /**
     * Find best selling product.
     */
    public static Map<String, Object> FindBestSellingProduct(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<GenericValue> salesOrderItemStarSchemas = null;
        GenericValue newEntity = null;
        GenericValue salesOrderItemStarSchema = null;
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        List<GenericValue> productCategoryMembers = null;
        try {
            productCategoryMembers = EntityQuery.use(delegator)
                    .from("ProductCategoryMember")
                    .where(UtilMisc.toMap("productCategoryId", context.get("productCategoryId")))
                    .filterByDate()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (productCategoryMembers != null) {
            for (GenericValue productCategoryMember : productCategoryMembers) {
                try {
                    Map<String, Object> scriptContext = new HashMap<String, Object>();
                    scriptContext.put("delegator", delegator);
                    scriptContext.put("dispatcher", dispatcher);
                    scriptContext.put("locale", locale);
                    scriptContext.put("userLogin", userLogin);
                    scriptContext.put("context", context);
                    scriptContext.put("parameters", context);
                    Object scriptResult = GroovyUtil.eval("entityDefined = false;\n                try {\n                    modelEntity = delegator.getModelReader().getModelEntity(\"SalesOrderItemStarSchema\");\n                    if (modelEntity) {\n                        entityDefined = true;\n                    }\n                } catch(Exception e) {\n                }\n                context.entityDefined = entityDefined;", scriptContext);
                } catch (Exception e) {
                    Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
                }
                if (Boolean.FALSE.equals(context.get("entityDefined"))) {
                    Debug.logWarning("No best-selling products can be returned using 'FindBestSellingProduct' service; entity SalesOrderItemStarSchema does not exist", MODULE);
                    return result;
                }
                try {
                    salesOrderItemStarSchemas = EntityQuery.use(delegator)
                            .from("SalesOrderItemStarSchema")
                            .distinct()
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying SalesOrderItemStarSchema: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (UtilValidate.isNotEmpty(salesOrderItemStarSchemas)) {
                    salesOrderItemStarSchema = EntityUtil.getFirst((List<GenericValue>) salesOrderItemStarSchemas);
                    newEntity = delegator.makeValue("ProductCategoryMember");
                    newEntity.put("productCategoryId", context.get("primaryProductCategoryId"));
                    newEntity.put("productId", salesOrderItemStarSchema.get("productProductId"));
                    newEntity.put("fromDate", nowTimestamp);
                    newEntity.put("quantity", salesOrderItemStarSchema.get("quantity"));
                    try {
                        delegator.create(newEntity);
                    } catch (Exception e) {
                        Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            }
        }

        return result;
    }


    /**
     */
    public static Map<String, Object> createProdCatalogAndStoreAssoc(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> createRecordCtx = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "createRecordCtx" for service "createProdCatalog"
        createRecordCtx.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createProdCatalog", createRecordCtx);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            context.put("prodCatalogId", serviceResult.get("prodCatalogId"));
            result.put("prodCatalogId", serviceResult.get("prodCatalogId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createProdCatalog: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> createAssocCtx = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "createAssocCtx" for service "createProductStoreCatalog"
        createAssocCtx.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createProductStoreCatalog", createAssocCtx);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            result.put("productStoreId", serviceResult.get("productStoreId"));
            result.put("fromDate", serviceResult.get("fromDate"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createProductStoreCatalog: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     */
    public static Map<String, Object> updateProdCatalogAndStoreAssoc(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> updateRecordCtx = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "updateRecordCtx" for service "updateProdCatalog"
        updateRecordCtx.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateProdCatalog", updateRecordCtx);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateProdCatalog: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> updateAssocCtx = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "updateAssocCtx" for service "updateProductStoreCatalog"
        updateAssocCtx.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateProductStoreCatalog", updateAssocCtx);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateProductStoreCatalog: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     */
    public static Map<String, Object> deleteProdCatalogAndStoreAssoc(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> deleteAssocCtx = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "deleteAssocCtx" for service "deleteProductStoreCatalog"
        deleteAssocCtx.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("deleteProductStoreCatalog", deleteAssocCtx);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling deleteProductStoreCatalog: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> deleteRecordCtx = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "deleteRecordCtx" for service "deleteProdCatalog"
        deleteRecordCtx.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("deleteProdCatalog", deleteRecordCtx);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling deleteProdCatalog: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     */
    public static Map<String, Object> createProductCategoryAndCatalogAssoc(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> createRecordCtx = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "createRecordCtx" for service "createProductCategory"
        createRecordCtx.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createProductCategory", createRecordCtx);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            context.put("productCategoryId", serviceResult.get("productCategoryId"));
            result.put("productCategoryId", serviceResult.get("productCategoryId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createProductCategory: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> createAssocCtx = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "createAssocCtx" for service "addProductCategoryToProdCatalog"
        createAssocCtx.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("addProductCategoryToProdCatalog", createAssocCtx);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            result.put("prodCatalogId", serviceResult.get("prodCatalogId"));
            result.put("prodCatalogCategoryTypeId", serviceResult.get("prodCatalogCategoryTypeId"));
            result.put("fromDate", serviceResult.get("fromDate"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling addProductCategoryToProdCatalog: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     */
    public static Map<String, Object> createProductCategoryAndCategoryAssoc(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> createRecordCtx = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "createRecordCtx" for service "createProductCategory"
        createRecordCtx.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createProductCategory", createRecordCtx);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            context.put("productCategoryId", serviceResult.get("productCategoryId"));
            result.put("productCategoryId", serviceResult.get("productCategoryId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createProductCategory: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> createAssocCtx = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "createAssocCtx" for service "addProductCategoryToCategory"
        createAssocCtx.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("addProductCategoryToCategory", createAssocCtx);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            result.put("parentProductCategoryId", serviceResult.get("parentProductCategoryId"));
            result.put("fromDate", serviceResult.get("fromDate"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling addProductCategoryToCategory: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     */
    public static Map<String, Object> updateProductCategoryAndCatalogAssoc(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> updateRecordCtx = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "updateRecordCtx" for service "updateProductCategory"
        updateRecordCtx.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateProductCategory", updateRecordCtx);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateProductCategory: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> updateAssocCtx = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "updateAssocCtx" for service "updateProductCategoryToProdCatalog"
        updateAssocCtx.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateProductCategoryToProdCatalog", updateAssocCtx);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateProductCategoryToProdCatalog: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     */
    public static Map<String, Object> updateProductCategoryAndCategoryAssoc(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> updateRecordCtx = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "updateRecordCtx" for service "updateProductCategory"
        updateRecordCtx.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateProductCategory", updateRecordCtx);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateProductCategory: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> updateAssocCtx = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "updateAssocCtx" for service "updateProductCategoryToCategory"
        updateAssocCtx.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateProductCategoryToCategory", updateAssocCtx);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateProductCategoryToCategory: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     */
    public static Map<String, Object> createProductCategoryAndCatAssocVersatile(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> createCtx = new HashMap<>();
        Map<String, Object> updateLocTextCtx = new HashMap<>();
        if (UtilValidate.isEmpty(context.get("parentProductCategoryId"))) {
            // set-service-fields from "parameters" to "createCtx" for service "createProductCategoryAndCatalogAssoc"
            createCtx.putAll(UtilMisc.toMap(context));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createProductCategoryAndCatalogAssoc", createCtx);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
                context.put("productCategoryId", serviceResult.get("productCategoryId"));
                result.put("productCategoryId", serviceResult.get("productCategoryId"));
                result.put("prodCatalogId", serviceResult.get("prodCatalogId"));
                result.put("prodCatalogCategoryTypeId", serviceResult.get("prodCatalogCategoryTypeId"));
                result.put("fromDate", serviceResult.get("fromDate"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling createProductCategoryAndCatalogAssoc: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        } else {
            // set-service-fields from "parameters" to "createCtx" for service "createProductCategoryAndCategoryAssoc"
            createCtx.putAll(UtilMisc.toMap(context));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createProductCategoryAndCategoryAssoc", createCtx);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
                context.put("productCategoryId", serviceResult.get("productCategoryId"));
                result.put("productCategoryId", serviceResult.get("productCategoryId"));
                result.put("parentProductCategoryId", serviceResult.get("parentProductCategoryId"));
                result.put("fromDate", serviceResult.get("fromDate"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling createProductCategoryAndCategoryAssoc: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        if (Boolean.TRUE.equals(context.get("updateLocalizedTexts"))) {
            // set-service-fields from "parameters" to "updateLocTextCtx" for service "replaceProductCategoryContentLocalizedSimpleTexts"
            updateLocTextCtx.putAll(UtilMisc.toMap(context));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("replaceProductCategoryContentLocalizedSimpleTexts", updateLocTextCtx);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling replaceProductCategoryContentLocalizedSimpleTexts: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        return result;
    }


    /**
     */
    public static Map<String, Object> updateProductCategoryAndCatAssocVersatile(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> updateCtx = new HashMap<>();
        Map<String, Object> updateLocTextCtx = new HashMap<>();
        if (UtilValidate.isEmpty(context.get("parentProductCategoryId"))) {
            // set-service-fields from "parameters" to "updateCtx" for service "updateProductCategoryAndCatalogAssoc"
            updateCtx.putAll(UtilMisc.toMap(context));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updateProductCategoryAndCatalogAssoc", updateCtx);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling updateProductCategoryAndCatalogAssoc: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        } else {
            // set-service-fields from "parameters" to "updateCtx" for service "updateProductCategoryAndCategoryAssoc"
            updateCtx.putAll(UtilMisc.toMap(context));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updateProductCategoryAndCategoryAssoc", updateCtx);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling updateProductCategoryAndCategoryAssoc: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        if (Boolean.TRUE.equals(context.get("updateLocalizedTexts"))) {
            // set-service-fields from "parameters" to "updateLocTextCtx" for service "replaceProductCategoryContentLocalizedSimpleTexts"
            updateLocTextCtx.putAll(UtilMisc.toMap(context));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("replaceProductCategoryContentLocalizedSimpleTexts", updateLocTextCtx);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling replaceProductCategoryContentLocalizedSimpleTexts: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        return result;
    }


    /**
     */
    public static Map<String, Object> deleteProductCategoryCatAssocVersatile(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> removeCtx = new HashMap<>();
        Map<String, Object> updateCtx = new HashMap<>();
        Timestamp updateCtx_thruDate = null;
        if (UtilValidate.isEmpty(context.get("parentProductCategoryId"))) {
            if ("expire".equals(context.get("deleteAssocMode"))) {
                // set-service-fields from "parameters" to "updateCtx" for service "updateProductCategoryToProdCatalog"
                updateCtx.putAll(UtilMisc.toMap(context));
                updateCtx_thruDate = new Timestamp(System.currentTimeMillis());
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("updateProductCategoryToProdCatalog", updateCtx);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling updateProductCategoryToProdCatalog: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            } else {
                // set-service-fields from "parameters" to "removeCtx" for service "removeProductCategoryFromProdCatalog"
                removeCtx.putAll(UtilMisc.toMap(context));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("removeProductCategoryFromProdCatalog", removeCtx);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling removeProductCategoryFromProdCatalog: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        } else {
            if ("expire".equals(context.get("deleteAssocMode"))) {
                // set-service-fields from "parameters" to "updateCtx" for service "updateProductCategoryToCategory"
                updateCtx.putAll(UtilMisc.toMap(context));
                updateCtx_thruDate = new Timestamp(System.currentTimeMillis());
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("updateProductCategoryToCategory", updateCtx);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling updateProductCategoryToCategory: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            } else {
                // set-service-fields from "parameters" to "removeCtx" for service "removeProductCategoryFromCategory"
                removeCtx.putAll(UtilMisc.toMap(context));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("removeProductCategoryFromCategory", removeCtx);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling removeProductCategoryFromCategory: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }

        return result;
    }


    /**
     */
    public static Map<String, Object> deleteProductCategoryAndRelatedVersatile(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        String errMsgStr = null;
        Object assocContentFilterByDate = null;
        Object contentRemoveCtx = null;
        List<GenericValue> recursiveContentList = null;
        List<GenericValue> values = null;
        Map<String, Object> parentProdCatIdMap = null;
        Object checkAction = null;
        List<GenericValue> emptyField = null;
        String callingMethodName = null;
        Object productCategoryIdName = null;
        Boolean hasPermission = null;
        List<GenericValue> roleCategories = null;
        Object productCategoryIdToCheck = null;
        callingMethodName = "deleteProductCategoryAndRelatedVersatile";
        checkAction = "DELETE";
        Map<String, Object> inlineResult = checkCategoryRelatedPermission(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue productCategory = null;
        try {
            productCategory = EntityQuery.use(delegator)
                    .from("ProductCategory")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ProductCategory: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(productCategory)) {
            errMsgStr = UtilProperties.getMessage("ProductUiLabels", "ProductCategoryNotFoundForCategoryID", locale);
            error_list.add("${errMsgStr}: ${parameters.productCategoryId}");
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        }
        Map<String, Object> prodCatIdMap = new HashMap<String, Object>();
        prodCatIdMap.put("productCategoryId", context.get("productCategoryId"));
        try {
            delegator.removeByAnd("ProductCategoryAttribute", prodCatIdMap);
        } catch (Exception e) {
            Debug.logError(e, "Error removing ProductCategoryAttribute: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            delegator.removeByAnd("ProductCategoryLink", prodCatIdMap);
        } catch (Exception e) {
            Debug.logError(e, "Error removing ProductCategoryLink: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            delegator.removeByAnd("ProductCategoryRole", prodCatIdMap);
        } catch (Exception e) {
            Debug.logError(e, "Error removing ProductCategoryRole: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            delegator.removeByAnd("ProductFeatureCategoryAppl", prodCatIdMap);
        } catch (Exception e) {
            Debug.logError(e, "Error removing ProductFeatureCategoryAppl: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            delegator.removeByAnd("ProductFeatureCatGrpAppl", prodCatIdMap);
        } catch (Exception e) {
            Debug.logError(e, "Error removing ProductFeatureCatGrpAppl: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            delegator.removeByAnd("ProductPromoCategory", prodCatIdMap);
        } catch (Exception e) {
            Debug.logError(e, "Error removing ProductPromoCategory: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        assocContentFilterByDate = null;
        if ("all".equals(context.get("deleteContentRecursive"))) {
            assocContentFilterByDate = "false";
        } else {
            if ("active".equals(context.get("deleteContentRecursive"))) {
                assocContentFilterByDate = "true";
            }
        }
        if (UtilValidate.isNotEmpty(assocContentFilterByDate)) {
            try {
                recursiveContentList = EntityQuery.use(delegator)
                        .from("ProductCategoryContent")
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ProductCategoryContent: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (recursiveContentList != null) {
                for (GenericValue prodCatContent : recursiveContentList) {
                    contentRemoveCtx = null;
                    ((Map<String, Object>) contentRemoveCtx).put("contentId", prodCatContent.get("contentId"));
                    ((Map<String, Object>) contentRemoveCtx).put("recursiveTarget", context.get("deleteContentRecursive"));
                    try {
                        delegator.removeValue(prodCatContent);
                    } catch (Exception e) {
                        Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    if (!error_list.isEmpty()) {
                        return ServiceUtil.returnError(error_list);
                    }
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("removeContentAndRelatedRecursiveTo", (Map<String, Object>) contentRemoveCtx);
                        if (ServiceUtil.isError(serviceResult)) {
                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling removeContentAndRelatedRecursiveTo: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            }
        }
        try {
            delegator.removeByAnd("ProductCategoryContent", prodCatIdMap);
        } catch (Exception e) {
            Debug.logError(e, "Error removing ProductCategoryContent: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if ("expired".equals(context.get("deleteParentAssocSelect"))) {
            try {
                values = EntityQuery.use(delegator)
                        .from("ProdCatalogCategory")
                        .where(UtilMisc.toMap("productCategoryId", ((Map<String, Object>) prodCatIdMap).get("productCategoryId")))
                        .filterByDate()
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            try {
                delegator.removeAll(values);
            } catch (Exception e) {
                Debug.logError(e, "Error removing list: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            try {
                values = EntityQuery.use(delegator)
                        .from("ProductCategoryRollup")
                        .where(UtilMisc.toMap("productCategoryId", ((Map<String, Object>) prodCatIdMap).get("productCategoryId")))
                        .filterByDate()
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            try {
                delegator.removeAll(values);
            } catch (Exception e) {
                Debug.logError(e, "Error removing list: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        if ("all".equals(context.get("deleteParentAssocSelect"))) {
            try {
                delegator.removeByAnd("ProdCatalogCategory", prodCatIdMap);
            } catch (Exception e) {
                Debug.logError(e, "Error removing ProdCatalogCategory: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            try {
                delegator.removeByAnd("ProductCategoryRollup", prodCatIdMap);
            } catch (Exception e) {
                Debug.logError(e, "Error removing ProductCategoryRollup: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        if ("recursive".equals(context.get("deleteChildrenSelect"))) {
        }
        if ("expired".equals(context.get("deleteChildAssocSelect"))) {
            try {
                values = EntityQuery.use(delegator)
                        .from("ProductCategoryMember")
                        .where(UtilMisc.toMap("productCategoryId", ((Map<String, Object>) prodCatIdMap).get("productCategoryId")))
                        .filterByDate()
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            try {
                delegator.removeAll(values);
            } catch (Exception e) {
                Debug.logError(e, "Error removing list: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            try {
                values = EntityQuery.use(delegator)
                        .from("ProductCategoryRollup")
                        .where(UtilMisc.toMap("parentProductCategoryId", ((Map<String, Object>) prodCatIdMap).get("productCategoryId")))
                        .filterByDate()
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            try {
                delegator.removeAll(values);
            } catch (Exception e) {
                Debug.logError(e, "Error removing list: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        if ("all".equals(context.get("deleteChildAssocSelect"))) {
            try {
                delegator.removeByAnd("ProductCategoryMember", prodCatIdMap);
            } catch (Exception e) {
                Debug.logError(e, "Error removing ProductCategoryMember: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            parentProdCatIdMap.put("productCategoryId", context.get("productCategoryId"));
            try {
                delegator.removeByAnd("ProductCategoryRollup", parentProdCatIdMap);
            } catch (Exception e) {
                Debug.logError(e, "Error removing ProductCategoryRollup: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        if ("expired".equals(context.get("deleteSpecialAssocSelect"))) {
            try {
                values = EntityQuery.use(delegator)
                        .from("TaxAuthorityRateProduct")
                        .where(UtilMisc.toMap("productCategoryId", ((Map<String, Object>) prodCatIdMap).get("productCategoryId")))
                        .filterByDate()
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            try {
                delegator.removeAll(values);
            } catch (Exception e) {
                Debug.logError(e, "Error removing list: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            try {
                values = EntityQuery.use(delegator)
                        .from("MarketInterest")
                        .where(UtilMisc.toMap("productCategoryId", ((Map<String, Object>) prodCatIdMap).get("productCategoryId")))
                        .filterByDate()
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            try {
                delegator.removeAll(values);
            } catch (Exception e) {
                Debug.logError(e, "Error removing list: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            try {
                values = EntityQuery.use(delegator)
                        .from("ProductStoreSurveyAppl")
                        .where(UtilMisc.toMap("productCategoryId", ((Map<String, Object>) prodCatIdMap).get("productCategoryId")))
                        .filterByDate()
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            try {
                delegator.removeAll(values);
            } catch (Exception e) {
                Debug.logError(e, "Error removing list: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            try {
                values = EntityQuery.use(delegator)
                        .from("Subscription")
                        .where(UtilMisc.toMap("productCategoryId", ((Map<String, Object>) prodCatIdMap).get("productCategoryId")))
                        .filterByDate()
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            try {
                delegator.removeAll(values);
            } catch (Exception e) {
                Debug.logError(e, "Error removing list: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        if ("all".equals(context.get("deleteSpecialAssocSelect"))) {
            try {
                delegator.removeByAnd("TaxAuthorityCategory", prodCatIdMap);
            } catch (Exception e) {
                Debug.logError(e, "Error removing TaxAuthorityCategory: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            try {
                delegator.removeByAnd("TaxAuthorityRateProduct", prodCatIdMap);
            } catch (Exception e) {
                Debug.logError(e, "Error removing TaxAuthorityRateProduct: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            try {
                delegator.removeByAnd("ProductCategoryGlAccount", prodCatIdMap);
            } catch (Exception e) {
                Debug.logError(e, "Error removing ProductCategoryGlAccount: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            try {
                delegator.removeByAnd("SalesForecastDetail", prodCatIdMap);
            } catch (Exception e) {
                Debug.logError(e, "Error removing SalesForecastDetail: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            try {
                delegator.removeByAnd("MarketInterest", prodCatIdMap);
            } catch (Exception e) {
                Debug.logError(e, "Error removing MarketInterest: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            try {
                delegator.removeByAnd("PartyNeed", prodCatIdMap);
            } catch (Exception e) {
                Debug.logError(e, "Error removing PartyNeed: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            try {
                delegator.removeByAnd("ProductStoreSurveyAppl", prodCatIdMap);
            } catch (Exception e) {
                Debug.logError(e, "Error removing ProductStoreSurveyAppl: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            try {
                delegator.removeByAnd("Subscription", prodCatIdMap);
            } catch (Exception e) {
                Debug.logError(e, "Error removing Subscription: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        try {
            delegator.removeValue(productCategory);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     */
    public static Map<String, Object> deleteProductCategoryAndCatAssocVersatile(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> deleteRecordCtx = new HashMap<>();
        Map<String, Object> deleteAssocCtx = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "deleteAssocCtx" for service "deleteProductCategoryCatAssocVersatile"
        deleteAssocCtx.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("deleteProductCategoryCatAssocVersatile", deleteAssocCtx);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling deleteProductCategoryCatAssocVersatile: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (!Boolean.FALSE.equals(context.get("deleteRecordAndRelated"))) {
            // set-service-fields from "parameters" to "deleteRecordCtx" for service "deleteProductCategoryAndRelatedVersatile"
            deleteRecordCtx.putAll(UtilMisc.toMap(context));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("deleteProductCategoryAndRelatedVersatile", deleteRecordCtx);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling deleteProductCategoryAndRelatedVersatile: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        return result;
    }


    /**
     */
    public static Map<String, Object> addProductCategoryCatAssocVersatile(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Object missingFieldName = null;
        List<GenericValue> assocList = null;
        Map<String, Object> createAssocCtx = new HashMap<>();
        if (UtilValidate.isEmpty(context.get("parentProductCategoryId"))) {
            if (UtilValidate.isEmpty(context.get("prodCatalogId"))) {
                missingFieldName = "prodCatalogId";
                {
                    String errorMsg = UtilProperties.getMessage("CommonErrorUiLabels", "CommonMissingFieldWithName", locale);
                    error_list.add(errorMsg);
                }
                if (!error_list.isEmpty()) {
                    return ServiceUtil.returnError(error_list);
                }
            }
            try {
                assocList = EntityQuery.use(delegator)
                        .from("ProdCatalogCategory")
                        .where(UtilMisc.toMap("productCategoryId", context.get("productCategoryId"), "prodCatalogId", context.get("prodCatalogId")))
                        .filterByDate()
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (UtilValidate.isNotEmpty(assocList)) {
                {
                    String errorMsg = UtilProperties.getMessage("ProductErrorUiLabels", "productservices.could_not_create_catalog_category_assoc_association_exists", locale);
                    error_list.add(errorMsg);
                }
                if (!error_list.isEmpty()) {
                    return ServiceUtil.returnError(error_list);
                }
            }
            // set-service-fields from "parameters" to "createAssocCtx" for service "addProductCategoryToProdCatalog"
            createAssocCtx.putAll(UtilMisc.toMap(context));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("addProductCategoryToProdCatalog", createAssocCtx);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
                result.put("productCategoryId", serviceResult.get("productCategoryId"));
                result.put("prodCatalogId", serviceResult.get("prodCatalogId"));
                result.put("prodCatalogCategoryTypeId", serviceResult.get("prodCatalogCategoryTypeId"));
                result.put("fromDate", serviceResult.get("fromDate"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling addProductCategoryToProdCatalog: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        } else {
            try {
                assocList = EntityQuery.use(delegator)
                        .from("ProductCategoryRollup")
                        .where(UtilMisc.toMap("productCategoryId", context.get("productCategoryId"), "parentProductCategoryId", context.get("parentProductCategoryId")))
                        .filterByDate()
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (UtilValidate.isNotEmpty(assocList)) {
                {
                    String errorMsg = UtilProperties.getMessage("ProductErrorUiLabels", "productservices.could_not_create_category_category_assoc_association_exists", locale);
                    error_list.add(errorMsg);
                }
                if (!error_list.isEmpty()) {
                    return ServiceUtil.returnError(error_list);
                }
            }
            // set-service-fields from "parameters" to "createAssocCtx" for service "addProductCategoryToCategory"
            createAssocCtx.putAll(UtilMisc.toMap(context));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("addProductCategoryToCategory", createAssocCtx);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
                result.put("productCategoryId", serviceResult.get("productCategoryId"));
                result.put("parentProductCategoryId", serviceResult.get("parentProductCategoryId"));
                result.put("fromDate", serviceResult.get("fromDate"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling addProductCategoryToCategory: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        return result;
    }


    /**
     */
    public static Map<String, Object> copyProductCategoryCatAssocVersatile(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> addAssocCtx = new HashMap<String, Object>();
        addAssocCtx.put("productCategoryId", context.get("productCategoryId"));
        addAssocCtx.put("fromDate", context.get("to_fromDate"));
        addAssocCtx.put("sequenceNum", context.get("to_sequenceNum"));
        addAssocCtx.put("prodCatalogId", context.get("to_prodCatalogId"));
        addAssocCtx.put("prodCatalogCategoryTypeId", context.get("to_prodCatalogCategoryTypeId"));
        addAssocCtx.put("parentProductCategoryId", context.get("to_parentProductCategoryId"));
        Map<String, Object> resultFields = new HashMap<String, Object>();
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("addProductCategoryCatAssocVersatile", addAssocCtx);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            resultFields.put("productCategoryId", serviceResult.get("productCategoryId"));
            resultFields.put("fromDate", serviceResult.get("fromDate"));
            resultFields.put("sequenceNum", serviceResult.get("sequenceNum"));
            resultFields.put("prodCatalogId", serviceResult.get("prodCatalogId"));
            resultFields.put("prodCatalogCategoryTypeId", serviceResult.get("prodCatalogCategoryTypeId"));
            resultFields.put("parentProductCategoryId", serviceResult.get("parentProductCategoryId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling addProductCategoryCatAssocVersatile: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        result.put("productCategoryId", ((Map<String, Object>) resultFields).get("productCategoryId"));
        if (Boolean.TRUE.equals(context.get("returnAssocFields"))) {
            result.put("fromDate", ((Map<String, Object>) resultFields).get("fromDate"));
            result.put("sequenceNum", ((Map<String, Object>) resultFields).get("sequenceNum"));
            result.put("prodCatalogId", ((Map<String, Object>) resultFields).get("prodCatalogId"));
            result.put("prodCatalogCategoryTypeId", ((Map<String, Object>) resultFields).get("prodCatalogCategoryTypeId"));
            result.put("parentProductCategoryId", ((Map<String, Object>) resultFields).get("parentProductCategoryId"));
        }
        result.put("to_productCategoryId", ((Map<String, Object>) resultFields).get("productCategoryId"));
        result.put("to_fromDate", ((Map<String, Object>) resultFields).get("fromDate"));
        result.put("to_sequenceNum", ((Map<String, Object>) resultFields).get("sequenceNum"));
        result.put("to_prodCatalogId", ((Map<String, Object>) resultFields).get("prodCatalogId"));
        result.put("to_prodCatalogCategoryTypeId", ((Map<String, Object>) resultFields).get("prodCatalogCategoryTypeId"));
        result.put("to_parentProductCategoryId", ((Map<String, Object>) resultFields).get("parentProductCategoryId"));

        return result;
    }


    /**
     */
    public static Map<String, Object> moveProductCategoryCatAssocVersatile(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Map<String, Object> addAssocCtx = new HashMap<>();
        Map<String, Object> resultFields = null;
        Map<String, Object> deleteAssocCtx = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "deleteAssocCtx" for service "deleteProductCategoryCatAssocVersatile"
        deleteAssocCtx.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("deleteProductCategoryCatAssocVersatile", deleteAssocCtx);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling deleteProductCategoryCatAssocVersatile: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> inlineResult = copyProductCategoryCatAssocVersatile(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }

        return result;
    }


    /**
     */
    public static Map<String, Object> getProductCategoryExtendedDataVersatile(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        String errMsgStr = null;
        GenericValue productCategory = null;
        try {
            productCategory = EntityQuery.use(delegator)
                    .from("ProductCategory")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ProductCategory: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        result.put("productCategory", productCategory);
        if (UtilValidate.isEmpty(productCategory)) {
            errMsgStr = UtilProperties.getMessage("ProductUiLabels", "ProductCategoryNotFoundForCategoryID", locale);
            error_list.add("${errMsgStr}: ${parameters.productCategoryId}");
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        }
        Map<String, Object> locTextCtx = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "locTextCtx" for service "getProductCategoryContentLocalizedSimpleTextViews"
        locTextCtx.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("getProductCategoryContentLocalizedSimpleTextViews", locTextCtx);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            result.put("viewsByType", serviceResult.get("viewsByType"));
            result.put("viewsByTypeAndLocale", serviceResult.get("viewsByTypeAndLocale"));
            result.put("textByTypeAndLocale", serviceResult.get("textByTypeAndLocale"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling getProductCategoryContentLocalizedSimpleTextViews: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }

}

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
import org.ofbiz.entity.serialize.XmlSerializer;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.product.product.KeywordIndex;
import org.ofbiz.product.product.ProductWorker;
import org.ofbiz.product.store.ProductStoreWorker;
import org.ofbiz.security.Security;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://product/script/org/ofbiz/product/product/ProductServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class ProductServices {

    private static final String MODULE = ProductServices.class.getName();


    /**
     * Create a Product
     */
    public static Map<String, Object> createProduct(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Security security = dctx.getSecurity();
        List<String> error_list = new LinkedList<>();

        GenericValue dummyProduct = null;
        GenericValue newEntity = null;
        List<GenericValue> productCategoryRoles = null;
        GenericValue newLimitMember = null;
        if (!security.hasEntityPermission("CATALOG", "_CREATE", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("ProductUiLabels", "ProductCatalogCreatePermissionError", locale));
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        newEntity = delegator.makeValue("Product");
        newEntity.setNonPKFields(context);
        newEntity.put("productId", context.get("productId"));
        if (UtilValidate.isEmpty(newEntity.get("productId"))) {
            ((GenericValue) newEntity).put("productId", delegator.getNextSeqId("Product"));
        } else {
            if (newEntity.get("productId") == null || ((String) newEntity.get("productId")).trim().isEmpty()) {
                error_list.add("Invalid ID for field newEntity.productId");
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
            try {
                dummyProduct = EntityQuery.use(delegator)
                        .from("Product")
                        .where(UtilMisc.toMap("productId", context.get("productId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying Product: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (UtilValidate.isNotEmpty(dummyProduct)) {
                {
                    String errorMsg = UtilProperties.getMessage("CommonErrorUiLabels", "CommonErrorDuplicateKey", locale);
                    error_list.add(errorMsg);
                }
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        }
        result.put("productId", newEntity.get("productId"));
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        newEntity.put("createdDate", nowTimestamp);
        newEntity.put("lastModifiedDate", nowTimestamp);
        newEntity.put("lastModifiedByUserLogin", userLogin.get("userLoginId"));
        newEntity.put("createdByUserLogin", userLogin.get("userLoginId"));
        if (UtilValidate.isEmpty(newEntity.get("isVariant"))) {
            newEntity.put("isVariant", "N");
        }
        if (UtilValidate.isEmpty(newEntity.get("isVirtual"))) {
            newEntity.put("isVirtual", "N");
        }
        if (UtilValidate.isEmpty(newEntity.get("billOfMaterialLevel"))) {
            newEntity.put("billOfMaterialLevel", 0L);
        }
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
                    newLimitMember = delegator.makeValue("ProductCategoryMember");
                    newLimitMember.put("productId", newEntity.get("productId"));
                    newLimitMember.put("productCategoryId", productCategoryRole.get("productCategoryId"));
                    newLimitMember.put("fromDate", nowTimestamp);
                    try {
                        delegator.create(newLimitMember);
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
     * Update a Product
     */
    public static Map<String, Object> updateProduct(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Map<String, Object> lookupRoleCategoriesMap = null;
        Object checkAction = null;
        List<GenericValue> emptyField = null;
        String callingMethodName = null;
        Object checkActionLabel = null;
        Object resourceDescription = null;
        List<GenericValue> roleCategories = null;
        callingMethodName = "updateProduct";
        checkAction = "UPDATE";
        Map<String, Object> inlineResult = checkProductRelatedPermission(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("Product")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Product: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> saveIdMap = new HashMap<String, Object>();
        saveIdMap.put("primaryProductCategoryId", lookedUpValue.get("primaryProductCategoryId"));
        lookedUpValue.setNonPKFields(context);
        Timestamp lookedUpValue_lastModifiedDate = new Timestamp(System.currentTimeMillis());
        lookedUpValue.put("lastModifiedByUserLogin", userLogin.get("userLoginId"));
        try {
            delegator.store(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Update a Product Name from quick admin
     */
    public static Map<String, Object> updateProductQuickAdminName(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue lookedUpValue = null;
        Map<String, Object> variantProductAssocMap = null;
        List<GenericValue> emptyField = null;
        List<GenericValue> variantProductAssocs = null;
        GenericValue variantProduct = null;
        Map<String, Object> lookupRoleCategoriesMap = null;
        Object checkAction = null;
        String callingMethodName = null;
        Object checkActionLabel = null;
        Object resourceDescription = null;
        List<GenericValue> roleCategories = null;
        callingMethodName = "updateProductQuickAdminName";
        checkAction = "UPDATE";
        Map<String, Object> inlineResult = checkProductRelatedPermission(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("Product")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Product: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        lookedUpValue.put("productName", context.get("productName"));
        if ("Y".equals(lookedUpValue.get("isVirtual"))) {
            lookedUpValue.put("internalName", lookedUpValue.get("productName"));
        }
        Timestamp lookedUpValue_lastModifiedDate = new Timestamp(System.currentTimeMillis());
        lookedUpValue.put("lastModifiedByUserLogin", userLogin.get("userLoginId"));
        try {
            delegator.store(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if ("Y".equals(lookedUpValue.get("isVirtual"))) {
            variantProductAssocMap.put("productId", context.get("productId"));
            variantProductAssocMap.put("productAssocTypeId", "PRODUCT_VARIANT");
            try {
                variantProductAssocs = EntityQuery.use(delegator)
                        .from("ProductAssoc")
                        .where(variantProductAssocMap)
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ProductAssoc: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            emptyField = EntityUtil.filterByDate(UtilGenerics.cast(variantProductAssocs));
            if (variantProductAssocs != null) {
                for (GenericValue variantProductAssoc : variantProductAssocs) {
                    variantProduct = null;
                    try {
                        variantProduct = EntityQuery.use(delegator)
                                .from("Product")
                                .where(UtilMisc.toMap("productId", variantProductAssoc.get("productIdTo")))
                                .queryOne();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying Product: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    variantProduct.put("productName", context.get("productName"));
                    Timestamp variantProduct_lastModifiedDate = new Timestamp(System.currentTimeMillis());
                    variantProduct.put("lastModifiedByUserLogin", userLogin.get("userLoginId"));
                    try {
                        delegator.store(variantProduct);
                    } catch (Exception e) {
                        Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            }
        }

        return result;
    }


    /**
     * Duplicate a Product
     */
    public static Map<String, Object> duplicateProduct(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue newProduct = null;
        GenericValue newTempValue = null;
        GenericValue foundValue = null;
        List<GenericValue> foundValues = null;
        GenericValue oldElectronicText = null;
        String newContentId = null;
        GenericValue clonedElectronicText = null;
        GenericValue clonedContent = null;
        String newDataResourceId = null;
        GenericValue oldDataResource = null;
        GenericValue clonedDataresource = null;
        GenericValue oldContent = null;
        Map<String, Object> lookupRoleCategoriesMap = null;
        Object checkAction = null;
        List<GenericValue> emptyField = null;
        String callingMethodName = null;
        Object checkActionLabel = null;
        Object resourceDescription = null;
        List<GenericValue> roleCategories = null;
        callingMethodName = "duplicateProduct";
        checkAction = "CREATE";
        Map<String, Object> inlineResult = checkProductRelatedPermission(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        checkAction = "DELETE";
        inlineResult = checkProductRelatedPermission(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        GenericValue dummyProduct = null;
        try {
            dummyProduct = EntityQuery.use(delegator)
                    .from("Product")
                    .where(UtilMisc.toMap("productId", context.get("productId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Product: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(dummyProduct)) {
            {
                String errorMsg = UtilProperties.getMessage("CommonErrorUiLabels", "CommonErrorDuplicateKey", locale);
                error_list.add(errorMsg);
            }
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue oldProduct = null;
        try {
            oldProduct = EntityQuery.use(delegator)
                    .from("Product")
                    .where(UtilMisc.toMap("productId", context.get("oldProductId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Product: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        newProduct = GenericValue.create((GenericValue) oldProduct);
        newProduct.put("productId", context.get("productId"));
        if (UtilValidate.isNotEmpty(context.get("newInternalName"))) {
            newProduct.put("internalName", context.get("newInternalName"));
        }
        if (UtilValidate.isNotEmpty(context.get("newProductName"))) {
            newProduct.put("productName", context.get("newProductName"));
        }
        if (UtilValidate.isNotEmpty(context.get("newDescription"))) {
            newProduct.put("description", context.get("newDescription"));
        }
        if (UtilValidate.isNotEmpty(context.get("newLongDescription"))) {
            newProduct.put("longDescription", context.get("newLongDescription"));
        }
        try {
            delegator.create(newProduct);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> productFindContext = new HashMap<String, Object>();
        productFindContext.put("productId", context.get("oldProductId"));
        Map<String, Object> reverseProductFindContext = new HashMap<String, Object>();
        reverseProductFindContext.put("productIdTo", context.get("oldProductId"));
        if (UtilValidate.isNotEmpty(context.get("duplicatePrices"))) {
            try {
                foundValues = EntityQuery.use(delegator)
                        .from("ProductPrice")
                        .where(productFindContext)
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ProductPrice: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (foundValues != null) {
                for (GenericValue foundValue_iter : foundValues) {
                    foundValue = foundValue_iter;
                    newTempValue = GenericValue.create((GenericValue) foundValue);
                    newTempValue.put("productId", context.get("productId"));
                    try {
                        delegator.create(newTempValue);
                    } catch (Exception e) {
                        Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            }
        }
        if (UtilValidate.isNotEmpty(context.get("duplicateIDs"))) {
            try {
                foundValues = EntityQuery.use(delegator)
                        .from("GoodIdentification")
                        .where(productFindContext)
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying GoodIdentification: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (foundValues != null) {
                for (GenericValue foundValue_iter : foundValues) {
                    foundValue = foundValue_iter;
                    newTempValue = GenericValue.create((GenericValue) foundValue);
                    newTempValue.put("productId", context.get("productId"));
                    try {
                        delegator.create(newTempValue);
                    } catch (Exception e) {
                        Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            }
        }
        if (UtilValidate.isNotEmpty(context.get("duplicateContent"))) {
            try {
                foundValues = EntityQuery.use(delegator)
                        .from("ProductContent")
                        .where(productFindContext)
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ProductContent: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (foundValues != null) {
                for (GenericValue foundValue_iter : foundValues) {
                    foundValue = foundValue_iter;
                    newTempValue = GenericValue.create((GenericValue) foundValue);
                    newTempValue.put("productId", context.get("productId"));
                    try {
                        delegator.create(newTempValue);
                    } catch (Exception e) {
                        Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            }
        }
        if (UtilValidate.isNotEmpty(context.get("duplicateCategoryMembers"))) {
            try {
                foundValues = EntityQuery.use(delegator)
                        .from("ProductCategoryMember")
                        .where(productFindContext)
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ProductCategoryMember: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (foundValues != null) {
                for (GenericValue foundValue_iter : foundValues) {
                    foundValue = foundValue_iter;
                    newTempValue = GenericValue.create((GenericValue) foundValue);
                    newTempValue.put("productId", context.get("productId"));
                    newContentId = delegator.getNextSeqId("Content");
                    try {
                        oldContent = EntityQuery.use(delegator)
                                .from("Content")
                                .where(UtilMisc.toMap("contentId", "${newTempValue.contentId}"))
                                .queryOne();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying Content: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    if (UtilValidate.isNotEmpty(oldContent)) {
                        clonedContent = GenericValue.create((GenericValue) oldContent);
                        clonedContent.put("contentId", newContentId);
                        newTempValue.put("contentId", newContentId);
                        try {
                            oldDataResource = EntityQuery.use(delegator)
                                    .from("DataResource")
                                    .where(UtilMisc.toMap("dataResourceId", "${clonedContent.dataResourceId}"))
                                    .queryOne();
                        } catch (Exception e) {
                            Debug.logError(e, "Error querying DataResource: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                        if (UtilValidate.isNotEmpty(oldDataResource)) {
                            newDataResourceId = delegator.getNextSeqId("DataResource");
                            clonedDataresource = GenericValue.create((GenericValue) oldDataResource);
                            clonedDataresource.put("dataResourceId", newDataResourceId);
                            clonedContent.put("dataResourceId", newDataResourceId);
                            try {
                                delegator.create(clonedDataresource);
                            } catch (Exception e) {
                                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                                return ServiceUtil.returnError(e.getMessage());
                            }
                            try {
                                oldElectronicText = oldDataResource.getRelatedOne("ElectronicText", false);
                            } catch (Exception e) {
                                Debug.logError(e, "Error getting related one ElectronicText: " + e.getMessage(), MODULE);
                                return ServiceUtil.returnError(e.getMessage());
                            }
                            if (UtilValidate.isNotEmpty(oldElectronicText)) {
                                clonedElectronicText = GenericValue.create((GenericValue) oldElectronicText);
                                clonedElectronicText.put("dataResourceId", newDataResourceId);
                                try {
                                    delegator.create(clonedElectronicText);
                                } catch (Exception e) {
                                    Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                                    return ServiceUtil.returnError(e.getMessage());
                                }
                            }
                        }
                        try {
                            delegator.create(clonedContent);
                        } catch (Exception e) {
                            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                    }
                    try {
                        delegator.create(newTempValue);
                    } catch (Exception e) {
                        Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            }
        }
        if (UtilValidate.isNotEmpty(context.get("duplicateAssocs"))) {
            try {
                foundValues = EntityQuery.use(delegator)
                        .from("ProductAssoc")
                        .where(productFindContext)
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ProductAssoc: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (foundValues != null) {
                for (GenericValue foundValue_iter : foundValues) {
                    foundValue = foundValue_iter;
                    newTempValue = GenericValue.create((GenericValue) foundValue);
                    newTempValue.put("productId", context.get("productId"));
                    try {
                        delegator.create(newTempValue);
                    } catch (Exception e) {
                        Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            }
            try {
                foundValues = EntityQuery.use(delegator)
                        .from("ProductAssoc")
                        .where(UtilMisc.toMap("productIdTo", context.get("oldProductId")))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (foundValues != null) {
                for (GenericValue foundValue_iter : foundValues) {
                    foundValue = foundValue_iter;
                    newTempValue = GenericValue.create((GenericValue) foundValue);
                    newTempValue.put("productIdTo", context.get("productId"));
                    try {
                        delegator.create(newTempValue);
                    } catch (Exception e) {
                        Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            }
        }
        if (UtilValidate.isNotEmpty(context.get("duplicateAttributes"))) {
            try {
                foundValues = EntityQuery.use(delegator)
                        .from("ProductAttribute")
                        .where(productFindContext)
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ProductAttribute: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (foundValues != null) {
                for (GenericValue foundValue_iter : foundValues) {
                    foundValue = foundValue_iter;
                    newTempValue = GenericValue.create((GenericValue) foundValue);
                    newTempValue.put("productId", context.get("productId"));
                    try {
                        delegator.create(newTempValue);
                    } catch (Exception e) {
                        Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            }
        }
        if (UtilValidate.isNotEmpty(context.get("duplicateFeatureAppls"))) {
            try {
                foundValues = EntityQuery.use(delegator)
                        .from("ProductFeatureAppl")
                        .where(productFindContext)
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ProductFeatureAppl: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (foundValues != null) {
                for (GenericValue foundValue_iter : foundValues) {
                    foundValue = foundValue_iter;
                    newTempValue = GenericValue.create((GenericValue) foundValue);
                    newTempValue.put("productId", context.get("productId"));
                    try {
                        delegator.create(newTempValue);
                    } catch (Exception e) {
                        Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            }
        }
        if (UtilValidate.isNotEmpty(context.get("duplicateInventoryItems"))) {
            try {
                foundValues = EntityQuery.use(delegator)
                        .from("InventoryItem")
                        .where(productFindContext)
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying InventoryItem: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (foundValues != null) {
                for (GenericValue foundValue_iter : foundValues) {
                    foundValue = foundValue_iter;
                    newTempValue = GenericValue.create((GenericValue) foundValue);
                    newTempValue.put("productId", context.get("productId"));
                    ((GenericValue) newTempValue).put("inventoryItemId", delegator.getNextSeqId("InventoryItem"));
                    try {
                        delegator.create(newTempValue);
                    } catch (Exception e) {
                        Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            }
        }
        if (UtilValidate.isNotEmpty(context.get("removePrices"))) {
            try {
                delegator.removeByAnd("ProductPrice", productFindContext);
            } catch (Exception e) {
                Debug.logError(e, "Error removing ProductPrice: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        if (UtilValidate.isNotEmpty(context.get("removeIDs"))) {
            try {
                delegator.removeByAnd("GoodIdentification", productFindContext);
            } catch (Exception e) {
                Debug.logError(e, "Error removing GoodIdentification: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        if (UtilValidate.isNotEmpty(context.get("removeContent"))) {
            try {
                delegator.removeByAnd("ProductContent", productFindContext);
            } catch (Exception e) {
                Debug.logError(e, "Error removing ProductContent: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        if (UtilValidate.isNotEmpty(context.get("removeCategoryMembers"))) {
            try {
                delegator.removeByAnd("ProductCategoryMember", productFindContext);
            } catch (Exception e) {
                Debug.logError(e, "Error removing ProductCategoryMember: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        if (UtilValidate.isNotEmpty(context.get("removeAssocs"))) {
            try {
                delegator.removeByAnd("ProductAssoc", productFindContext);
            } catch (Exception e) {
                Debug.logError(e, "Error removing ProductAssoc: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            try {
                delegator.removeByAnd("ProductAssoc", reverseProductFindContext);
            } catch (Exception e) {
                Debug.logError(e, "Error removing ProductAssoc: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        if (UtilValidate.isNotEmpty(context.get("removeAttributes"))) {
            try {
                delegator.removeByAnd("ProductAttribute", productFindContext);
            } catch (Exception e) {
                Debug.logError(e, "Error removing ProductAttribute: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        if (UtilValidate.isNotEmpty(context.get("removeFeatureAppls"))) {
            try {
                delegator.removeByAnd("ProductFeatureAppl", productFindContext);
            } catch (Exception e) {
                Debug.logError(e, "Error removing ProductFeatureAppl: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        if (UtilValidate.isNotEmpty(context.get("removeInventoryItems"))) {
            try {
                delegator.removeByAnd("InventoryItem", productFindContext);
            } catch (Exception e) {
                Debug.logError(e, "Error removing InventoryItem: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        return result;
    }


    /**
     * induce all the keywords of a product
     */
    public static Map<String, Object> forceIndexProductKeywords(DispatchContext dctx, Map<String, Object> context) {
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
        try {
            KeywordIndex.forceIndexKeywords((GenericValue) product);
        } catch (Exception e) {
            Debug.logError(e, "Error calling KeywordIndex.forceIndexKeywords: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * delete all the keywords of a product
     */
    public static Map<String, Object> deleteProductKeywords(DispatchContext dctx, Map<String, Object> context) {
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
        try {
            product.removeRelated("ProductKeyword");
        } catch (Exception e) {
            Debug.logError(e, "Error removing related ProductKeyword: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Index the Keywords for a Product
     */
    public static Map<String, Object> indexProductKeywords(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue productInstance = null;
        Map<String, Object> findProductMap = null;
        Object emptyField = null;
        productInstance = (GenericValue) context.get("productInstance");
        if (UtilValidate.isEmpty(productInstance)) {
            findProductMap.put("productId", context.get("productId"));
            try {
                productInstance = EntityQuery.use(delegator)
                        .from("Product")
                        .where(findProductMap)
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error finding by primary key Product: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        if ((UtilValidate.isEmpty(productInstance.get("autoCreateKeywords")) || "Y".equals(productInstance.get("autoCreateKeywords")))) {
            try {
                KeywordIndex.indexKeywords((GenericValue) productInstance);
            } catch (Exception e) {
                Debug.logError(e, "Error calling KeywordIndex.indexKeywords: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        return result;
    }


    /**
     * Discontinue Product Sales
     */
    public static Map<String, Object> discontinueProductSales(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

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
        product.put("salesDiscontinuationDate", nowTimestamp);
        try {
            delegator.store(product);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        List<GenericValue> productCategoryMembers = null;
        try {
            productCategoryMembers = product.getRelated("ProductCategoryMember", null, null, false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related ProductCategoryMember: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (productCategoryMembers != null) {
            for (GenericValue productCategoryMember : productCategoryMembers) {
                if (UtilValidate.isEmpty(productCategoryMember.get("thruDate"))) {
                    productCategoryMember.put("thruDate", nowTimestamp);
                    try {
                        delegator.store(productCategoryMember);
                    } catch (Exception e) {
                        Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            }
        }
        List<GenericValue> assocProductAssocs = null;
        try {
            assocProductAssocs = product.getRelated("AssocProductAssoc", null, null, false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related AssocProductAssoc: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (assocProductAssocs != null) {
            for (GenericValue assocProductAssoc : assocProductAssocs) {
                if (UtilValidate.isEmpty(assocProductAssoc.get("thruDate"))) {
                    assocProductAssoc.put("thruDate", nowTimestamp);
                    try {
                        delegator.store(assocProductAssoc);
                    } catch (Exception e) {
                        Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            }
        }

        return result;
    }


    /**
     * Count Product View
     */
    public static Map<String, Object> countProductView(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue productCalculatedInfo = null;
        Map<String, Object> callSubMap = null;
        if (UtilValidate.isEmpty(context.get("weight"))) {
            ((Map<String, Object>) context).put("weight", 1);
        }
        try {
            productCalculatedInfo = EntityQuery.use(delegator)
                    .from("ProductCalculatedInfo")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ProductCalculatedInfo: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(productCalculatedInfo)) {
            productCalculatedInfo = delegator.makeValue("ProductCalculatedInfo");
            productCalculatedInfo.put("productId", context.get("productId"));
            productCalculatedInfo.put("totalTimesViewed", context.get("weight"));
            try {
                delegator.create(productCalculatedInfo);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        } else {
            productCalculatedInfo.set("totalTimesViewed", (new BigDecimal(context.get("weight").toString())).longValue());
            try {
                delegator.store(productCalculatedInfo);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
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
        Object virtualProductId = null;
        try {
            virtualProductId = ProductWorker.getVariantVirtualId((GenericValue) product);
        } catch (Exception e) {
            Debug.logError(e, "Error calling ProductWorker.getVariantVirtualId: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(virtualProductId)) {
            callSubMap.put("productId", virtualProductId);
            callSubMap.put("weight", context.get("weight"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("countProductView", callSubMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling countProductView: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        return result;
    }


    /**
     * Create a ProductReview
     */
    public static Map<String, Object> createProductReview(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Object proofOfPurchase = null;
        GenericValue newEntity = null;
        Object averageCustomerRating = null;
        GenericValue productCalculatedInfo = null;
        newEntity = delegator.makeValue("ProductReview");
        newEntity.setNonPKFields(context);
        newEntity.put("userLoginId", userLogin.get("userLoginId"));
        newEntity.put("statusId", "PRR_PENDING");
        GenericValue productStore = null;
        try {
            productStore = EntityQuery.use(delegator)
                    .from("ProductStore")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ProductStore: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(productStore)) {
            if ("Y".equals(productStore.get("reviewsPurchased"))) {
                try {
                    proofOfPurchase = ProductStoreWorker.proofOfPurchase(delegator, (GenericValue) productStore, (String) userLogin.get("partyId"), (String) newEntity.get("productId"));
                } catch (Exception e) {
                    Debug.logError(e, "Error calling ProductStoreWorker.proofOfPurchase: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (!Boolean.TRUE.equals(proofOfPurchase)) {
                    {
                        String errorMsg = UtilProperties.getMessage("ProductErrorUiLabels", "ProductReviewProofOfPurchaseFailed", locale);
                        error_list.add(errorMsg);
                    }
                    if (!error_list.isEmpty()) {
                        return ServiceUtil.returnError(error_list);
                    }
                }
            }
            if ("N".equals(productStore.get("multipleReviews"))) {
                try {
                    Map<String, Object> scriptContext = new HashMap<String, Object>();
                    scriptContext.put("delegator", delegator);
                    scriptContext.put("dispatcher", dispatcher);
                    scriptContext.put("locale", locale);
                    scriptContext.put("userLogin", userLogin);
                    scriptContext.put("context", context);
                    scriptContext.put("parameters", context);
                    Object scriptResult = GroovyUtil.eval("import org.ofbiz.entity.GenericValue;\n                    import org.ofbiz.entity.util.EntityQuery;\n                    import org.ofbiz.entity.condition.EntityCondition;\n                    import org.ofbiz.entity.condition.EntityOperator;\n                    import org.ofbiz.base.util.Debug;\n                    import org.ofbiz.base.util.UtilMisc;\n\n                    context.poductReviewAllowed = false;\n                    condition = EntityCondition.makeCondition(UtilMisc.toList(\n                        EntityCondition.makeCondition(\"userLoginId\", userLogin.userLoginId),\n                        EntityCondition.makeCondition(\"productId\", newEntity.productId),\n                        EntityCondition.makeCondition(\"productStoreId\", productStore.getString(\"productStoreId\")),\n                        EntityCondition.makeCondition(UtilMisc.toList(\n                            EntityCondition.makeCondition(\"postedAnonymous\", EntityOperator.EQUALS, null),\n                            EntityCondition.makeCondition(\"postedAnonymous\", EntityOperator.EQUALS, \"N\")), EntityOperator.OR)\n                    ), EntityOperator.AND);\n\n                    try {\n                        productReviewCount = EntityQuery.use(delegator)\n                            .from(\"ProductReview\")\n                            .where(condition).queryCount();\n                        if (productReviewCount == 0) {\n                            context.poductReviewAllowed = true;\n                        }\n                    } catch (Exception e) {\n                        Debug.logError(e.getMessage(), module);\n                    }", scriptContext);
                } catch (Exception e) {
                    Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
                }
                if ("false".equals(context.get("poductReviewAllowed"))) {
                    {
                        String errorMsg = UtilProperties.getMessage("ProductErrorUiLabels", "MultipleProductReviewsNotAllowed", locale);
                        error_list.add(errorMsg);
                    }
                    if (!error_list.isEmpty()) {
                        return ServiceUtil.returnError(error_list);
                    }
                }
            }
            if ("Y".equals(productStore.get("autoApproveReviews"))) {
                newEntity.put("statusId", "PRR_APPROVED");
            }
        }
        if (UtilValidate.isEmpty(context.get("productReview"))) {
            newEntity.put("statusId", "PRR_APPROVED");
        }
        ((GenericValue) newEntity).put("productReviewId", delegator.getNextSeqId("ProductReview"));
        result.put("productReviewId", newEntity.get("productReviewId"));
        if (UtilValidate.isEmpty(newEntity.get("postedDateTime"))) {
            Timestamp newEntity_postedDateTime = new Timestamp(System.currentTimeMillis());
        }
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Object productId = newEntity.get("productId");
        Object successMessage = UtilProperties.getMessage("ProductUiLabels", "ProductCreateProductReviewSuccess", locale);
        Map<String, Object> inlineResult = updateProductWithReviewRatingAvg(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }

        return result;
    }


    /**
     * Update ProductReview
     */
    public static Map<String, Object> updateProductReview(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Map<String, Object> lookupRoleCategoriesMap = null;
        Object checkAction = null;
        List<GenericValue> emptyField = null;
        String callingMethodName = null;
        Object checkActionLabel = null;
        Object resourceDescription = null;
        List<GenericValue> roleCategories = null;
        Object averageCustomerRating = null;
        GenericValue productCalculatedInfo = null;
        callingMethodName = "updateProductReview";
        checkAction = "UPDATE";
        Map<String, Object> inlineResult = checkProductRelatedPermission(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue lookupPKMap = delegator.makeValue("ProductReview");
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
        Object productId = lookedUpValue.get("productId");
        inlineResult = updateProductWithReviewRatingAvg(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }

        return result;
    }


    /**
     * Update Product with new Review Rating Avg
     */
    public static Map<String, Object> updateProductWithReviewRatingAvg(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue productCalculatedInfo = null;
        Object averageCustomerRating = null;
        try {
            averageCustomerRating = ProductWorker.getAverageProductRating(delegator, (String) context.get("productId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling ProductWorker.getAverageProductRating: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Debug.logInfo("Got new average customer rating " + averageCustomerRating, MODULE);
        if (java.util.Objects.equals(averageCustomerRating, BigDecimal.ZERO)) {
            return result;
        }
        try {
            productCalculatedInfo = EntityQuery.use(delegator)
                    .from("ProductCalculatedInfo")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ProductCalculatedInfo: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(productCalculatedInfo)) {
            productCalculatedInfo = delegator.makeValue("ProductCalculatedInfo");
            productCalculatedInfo.put("productId", context.get("productId"));
            productCalculatedInfo.put("averageCustomerRating", averageCustomerRating);
            try {
                delegator.create(productCalculatedInfo);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        } else {
            productCalculatedInfo.put("averageCustomerRating", averageCustomerRating);
            try {
                delegator.store(productCalculatedInfo);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        return result;
    }


    /**
     * Updates the Product's Variants
     */
    public static Map<String, Object> copyToProductVariants(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue newTempValue = null;
        List<GenericValue> foundVariantValues = null;
        List<GenericValue> foundValues = null;
        Map<String, Object> productVariantContext = null;
        Map<String, Object> lookupRoleCategoriesMap = null;
        Object checkAction = null;
        List<GenericValue> emptyField = null;
        String callingMethodName = null;
        Object checkActionLabel = null;
        Object resourceDescription = null;
        List<GenericValue> roleCategories = null;
        callingMethodName = "copyToProductVariants";
        checkAction = "CREATE";
        Map<String, Object> inlineResult = checkProductRelatedPermission(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        checkAction = "DELETE";
        inlineResult = checkProductRelatedPermission(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        Map<String, Object> productFindContext = new HashMap<String, Object>();
        productFindContext.put("productId", context.get("virtualProductId"));
        GenericValue oldProduct = null;
        try {
            oldProduct = EntityQuery.use(delegator)
                    .from("Product")
                    .where(productFindContext)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key Product: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> variantsFindContext = new HashMap<String, Object>();
        variantsFindContext.put("productId", context.get("virtualProductId"));
        variantsFindContext.put("productAssocTypeId", "PRODUCT_VARIANT");
        List<GenericValue> variants = null;
        try {
            variants = EntityQuery.use(delegator)
                    .from("ProductAssoc")
                    .where(variantsFindContext)
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ProductAssoc: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        emptyField = EntityUtil.filterByDate(UtilGenerics.cast(variants));
        if (variants != null) {
            for (GenericValue newProduct : variants) {
                productVariantContext.put("productId", newProduct.get("productIdTo"));
                if (UtilValidate.isNotEmpty(context.get("duplicatePrices"))) {
                    if (UtilValidate.isNotEmpty(context.get("removeBefore"))) {
                        try {
                            foundVariantValues = EntityQuery.use(delegator)
                                    .from("ProductPrice")
                                    .where(productVariantContext)
                                    .queryList();
                        } catch (Exception e) {
                            Debug.logError(e, "Error querying ProductPrice: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                        if (foundVariantValues != null) {
                            for (GenericValue foundVariantValue : foundVariantValues) {
                                try {
                                    delegator.removeValue(foundVariantValue);
                                } catch (Exception e) {
                                    Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                                    return ServiceUtil.returnError(e.getMessage());
                                }
                            }
                        }
                    }
                    try {
                        foundValues = EntityQuery.use(delegator)
                                .from("ProductPrice")
                                .where(productFindContext)
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying ProductPrice: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    if (foundValues != null) {
                        for (GenericValue foundValue : foundValues) {
                            newTempValue = GenericValue.create((GenericValue) foundValue);
                            newTempValue.put("productId", newProduct.get("productIdTo"));
                            try {
                                delegator.create(newTempValue);
                            } catch (Exception e) {
                                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                                return ServiceUtil.returnError(e.getMessage());
                            }
                        }
                    }
                }
                if (UtilValidate.isNotEmpty(context.get("duplicateIDs"))) {
                    if (UtilValidate.isNotEmpty(context.get("removeBefore"))) {
                        try {
                            foundVariantValues = EntityQuery.use(delegator)
                                    .from("GoodIdentification")
                                    .where(productVariantContext)
                                    .queryList();
                        } catch (Exception e) {
                            Debug.logError(e, "Error querying GoodIdentification: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                        if (foundVariantValues != null) {
                            for (GenericValue foundVariantValueEntry : foundVariantValues) {
                                try {
                                    delegator.removeValue(foundVariantValueEntry);
                                } catch (Exception e) {
                                    Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                                    return ServiceUtil.returnError(e.getMessage());
                                }
                            }
                        }
                    }
                    try {
                        foundValues = EntityQuery.use(delegator)
                                .from("GoodIdentification")
                                .where(productFindContext)
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying GoodIdentification: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    if (foundValues != null) {
                        for (GenericValue foundValueEntry : foundValues) {
                            newTempValue = GenericValue.create((GenericValue) foundValueEntry);
                            newTempValue.put("productId", newProduct.get("productIdTo"));
                            try {
                                delegator.create(newTempValue);
                            } catch (Exception e) {
                                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                                return ServiceUtil.returnError(e.getMessage());
                            }
                        }
                    }
                }
                if (UtilValidate.isNotEmpty(context.get("duplicateContent"))) {
                    if (UtilValidate.isNotEmpty(context.get("removeBefore"))) {
                        try {
                            foundVariantValues = EntityQuery.use(delegator)
                                    .from("ProductContent")
                                    .where(productVariantContext)
                                    .queryList();
                        } catch (Exception e) {
                            Debug.logError(e, "Error querying ProductContent: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                        if (foundVariantValues != null) {
                            for (GenericValue foundVariantValueEntry : foundVariantValues) {
                                try {
                                    delegator.removeValue(foundVariantValueEntry);
                                } catch (Exception e) {
                                    Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                                    return ServiceUtil.returnError(e.getMessage());
                                }
                            }
                        }
                    }
                    try {
                        foundValues = EntityQuery.use(delegator)
                                .from("ProductContent")
                                .where(productFindContext)
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying ProductContent: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    if (foundValues != null) {
                        for (GenericValue foundValueEntry : foundValues) {
                            newTempValue = GenericValue.create((GenericValue) foundValueEntry);
                            newTempValue.put("productId", newProduct.get("productIdTo"));
                            try {
                                delegator.create(newTempValue);
                            } catch (Exception e) {
                                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                                return ServiceUtil.returnError(e.getMessage());
                            }
                        }
                    }
                }
                if (UtilValidate.isNotEmpty(context.get("duplicateCategoryMembers"))) {
                    if (UtilValidate.isNotEmpty(context.get("removeBefore"))) {
                        try {
                            foundVariantValues = EntityQuery.use(delegator)
                                    .from("ProductCategoryMember")
                                    .where(productVariantContext)
                                    .queryList();
                        } catch (Exception e) {
                            Debug.logError(e, "Error querying ProductCategoryMember: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                        if (foundVariantValues != null) {
                            for (GenericValue foundVariantValueEntry : foundVariantValues) {
                                try {
                                    delegator.removeValue(foundVariantValueEntry);
                                } catch (Exception e) {
                                    Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                                    return ServiceUtil.returnError(e.getMessage());
                                }
                            }
                        }
                    }
                    try {
                        foundValues = EntityQuery.use(delegator)
                                .from("ProductCategoryMember")
                                .where(productFindContext)
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying ProductCategoryMember: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    if (foundValues != null) {
                        for (GenericValue foundValueEntry : foundValues) {
                            newTempValue = GenericValue.create((GenericValue) foundValueEntry);
                            newTempValue.put("productId", newProduct.get("productIdTo"));
                            try {
                                delegator.create(newTempValue);
                            } catch (Exception e) {
                                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                                return ServiceUtil.returnError(e.getMessage());
                            }
                        }
                    }
                }
                if (UtilValidate.isNotEmpty(context.get("duplicateAttributes"))) {
                    if (UtilValidate.isNotEmpty(context.get("removeBefore"))) {
                        try {
                            foundVariantValues = EntityQuery.use(delegator)
                                    .from("ProductAttribute")
                                    .where(productVariantContext)
                                    .queryList();
                        } catch (Exception e) {
                            Debug.logError(e, "Error querying ProductAttribute: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                        if (foundVariantValues != null) {
                            for (GenericValue foundVariantValueEntry : foundVariantValues) {
                                try {
                                    delegator.removeValue(foundVariantValueEntry);
                                } catch (Exception e) {
                                    Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                                    return ServiceUtil.returnError(e.getMessage());
                                }
                            }
                        }
                    }
                    try {
                        foundValues = EntityQuery.use(delegator)
                                .from("ProductAttribute")
                                .where(productFindContext)
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying ProductAttribute: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    if (foundValues != null) {
                        for (GenericValue foundValueEntry : foundValues) {
                            newTempValue = GenericValue.create((GenericValue) foundValueEntry);
                            newTempValue.put("productId", newProduct.get("productIdTo"));
                            try {
                                delegator.create(newTempValue);
                            } catch (Exception e) {
                                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                                return ServiceUtil.returnError(e.getMessage());
                            }
                        }
                    }
                }
                if (UtilValidate.isNotEmpty(context.get("duplicateFacilities"))) {
                    if (UtilValidate.isNotEmpty(context.get("removeBefore"))) {
                        try {
                            foundVariantValues = EntityQuery.use(delegator)
                                    .from("ProductFacility")
                                    .where(productVariantContext)
                                    .queryList();
                        } catch (Exception e) {
                            Debug.logError(e, "Error querying ProductFacility: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                        if (foundVariantValues != null) {
                            for (GenericValue foundVariantValueEntry : foundVariantValues) {
                                try {
                                    delegator.removeValue(foundVariantValueEntry);
                                } catch (Exception e) {
                                    Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                                    return ServiceUtil.returnError(e.getMessage());
                                }
                            }
                        }
                    }
                    try {
                        foundValues = EntityQuery.use(delegator)
                                .from("ProductFacility")
                                .where(productFindContext)
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying ProductFacility: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    if (foundValues != null) {
                        for (GenericValue foundValueEntry : foundValues) {
                            newTempValue = GenericValue.create((GenericValue) foundValueEntry);
                            newTempValue.put("productId", newProduct.get("productIdTo"));
                            try {
                                delegator.create(newTempValue);
                            } catch (Exception e) {
                                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                                return ServiceUtil.returnError(e.getMessage());
                            }
                        }
                    }
                }
                if (UtilValidate.isNotEmpty(context.get("duplicateLocations"))) {
                    if (UtilValidate.isNotEmpty(context.get("removeBefore"))) {
                        try {
                            foundVariantValues = EntityQuery.use(delegator)
                                    .from("ProductFacilityLocation")
                                    .where(productVariantContext)
                                    .queryList();
                        } catch (Exception e) {
                            Debug.logError(e, "Error querying ProductFacilityLocation: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                        if (foundVariantValues != null) {
                            for (GenericValue foundVariantValueEntry : foundVariantValues) {
                                try {
                                    delegator.removeValue(foundVariantValueEntry);
                                } catch (Exception e) {
                                    Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                                    return ServiceUtil.returnError(e.getMessage());
                                }
                            }
                        }
                    }
                    try {
                        foundValues = EntityQuery.use(delegator)
                                .from("ProductFacilityLocation")
                                .where(productFindContext)
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying ProductFacilityLocation: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    if (foundValues != null) {
                        for (GenericValue foundValueEntry : foundValues) {
                            newTempValue = GenericValue.create((GenericValue) foundValueEntry);
                            newTempValue.put("productId", newProduct.get("productIdTo"));
                            try {
                                delegator.create(newTempValue);
                            } catch (Exception e) {
                                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                                return ServiceUtil.returnError(e.getMessage());
                            }
                        }
                    }
                }
            }
        }

        return result;
    }


    /**
     * Check Product Related Permission
     */
    public static Map<String, Object> checkProductRelatedPermission(DispatchContext dctx, Map<String, Object> context) {
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
        Map<String, Object> lookupRoleCategoriesMap = null;
        List<GenericValue> emptyField = null;
        List<GenericValue> roleCategories = null;
        Object checkActionLabel = null;
        Object resourceDescription = null;
        if (UtilValidate.isEmpty(callingMethodName)) {
            callingMethodName = UtilProperties.getMessage("CommonUiLabels", "CommonPermissionThisOperation", locale);
        }
        if (UtilValidate.isEmpty(checkAction)) {
            checkAction = "UPDATE";
        }
        Object lookupRoleCategoriesMap_productId = null;
        Object lookupRoleCategoriesMap_partyId = null;
        Object lookupRoleCategoriesMap_roleTypeId = null;
        if (!(security.hasEntityPermission("CATALOG", "_${checkAction}", userLogin))) {
            lookupRoleCategoriesMap.put("productId", context.get("productId"));
            lookupRoleCategoriesMap.put("partyId", userLogin.get("partyId"));
            lookupRoleCategoriesMap.put("roleTypeId", "LTD_ADMIN");
            try {
                roleCategories = EntityQuery.use(delegator)
                        .from("ProductCategoryMemberAndRole")
                        .where(lookupRoleCategoriesMap)
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ProductCategoryMemberAndRole: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            emptyField = EntityUtil.filterByDate(UtilGenerics.cast(roleCategories));
            emptyField = EntityUtil.filterByDate(UtilGenerics.cast(roleCategories));
        }
        if (!((security.hasEntityPermission("CATALOG", "_${checkAction}", userLogin) || (security.hasEntityPermission("CATALOG_ROLE", "_${checkAction}", userLogin) && !(UtilValidate.isEmpty(roleCategories))) || (!(UtilValidate.isEmpty(alternatePermissionRoot)) && security.hasEntityPermission("${alternatePermissionRoot}", "_${checkAction}", userLogin))))) {
            checkActionLabel = "" + GroovyUtil.eval("'ProductCatalog' + checkAction.charAt(0) + checkAction.substring(1).toLowerCase() + 'PermissionError'", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
            resourceDescription = callingMethodName;
            {
                String errorMsg = UtilProperties.getMessage("ProductUiLabels", "${checkActionLabel}", locale);
                error_list.add(errorMsg);
            }
        }

        return result;
    }


    /**
     * Main permission logic
     */
    public static Map<String, Object> productGenericPermission(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Boolean hasPermission = null;
        String failMessage = null;
        Map<String, Object> lookupRoleCategoriesMap = null;
        Object checkAction = null;
        List<GenericValue> emptyField = null;
        String callingMethodName = null;
        Object checkActionLabel = null;
        Object resourceDescription = null;
        List<GenericValue> roleCategories = null;
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
        Map<String, Object> inlineResult = checkProductRelatedPermission(dctx, context);
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
     * product price permission logic
     */
    public static Map<String, Object> productPriceGenericPermission(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Security security = dctx.getSecurity();
        List<String> error_list = new LinkedList<>();

        Boolean hasPermission = null;
        String failMessage = null;
        Map<String, Object> lookupRoleCategoriesMap = null;
        Object checkAction = null;
        List<GenericValue> emptyField = null;
        String callingMethodName = null;
        Object checkActionLabel = null;
        Object resourceDescription = null;
        List<GenericValue> roleCategories = null;
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
        if (!security.hasPermission("CATALOG_PRICE_MAINT", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("ProductUiLabels", "ProductPriceMaintPermissionError", locale));
        }
        Map<String, Object> inlineResult = checkProductRelatedPermission(dctx, context);
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
     * Add Party to Product
     */
    public static Map<String, Object> addPartyToProduct(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Map<String, Object> lookupRoleCategoriesMap = null;
        Object checkAction = null;
        List<GenericValue> emptyField = null;
        String callingMethodName = null;
        Object checkActionLabel = null;
        Object resourceDescription = null;
        List<GenericValue> roleCategories = null;
        callingMethodName = "addPartyToProduct";
        checkAction = "CREATE";
        Map<String, Object> inlineResult = checkProductRelatedPermission(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue newEntity = delegator.makeValue("ProductRole");
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
     * Update Party to Product
     */
    public static Map<String, Object> updatePartyToProduct(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Map<String, Object> lookupRoleCategoriesMap = null;
        Object checkAction = null;
        List<GenericValue> emptyField = null;
        String callingMethodName = null;
        Object checkActionLabel = null;
        Object resourceDescription = null;
        List<GenericValue> roleCategories = null;
        callingMethodName = "updatePartyToProduct";
        checkAction = "UPDATE";
        Map<String, Object> inlineResult = checkProductRelatedPermission(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue lookupPKMap = delegator.makeValue("ProductRole");
        lookupPKMap.setPKFields(context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ProductRole")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ProductRole: " + e.getMessage(), MODULE);
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
     * Remove Party From Product
     */
    public static Map<String, Object> removePartyFromProduct(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Map<String, Object> lookupRoleCategoriesMap = null;
        Object checkAction = null;
        List<GenericValue> emptyField = null;
        String callingMethodName = null;
        Object checkActionLabel = null;
        Object resourceDescription = null;
        List<GenericValue> roleCategories = null;
        callingMethodName = "removePartyFromProduct";
        checkAction = "DELETE";
        Map<String, Object> inlineResult = checkProductRelatedPermission(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue lookupPKMap = delegator.makeValue("ProductRole");
        lookupPKMap.setPKFields(context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ProductRole")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ProductRole: " + e.getMessage(), MODULE);
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
     * Create a VendorProduct
     */
    public static Map<String, Object> createVendorProduct(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue newEntity = delegator.makeValue("VendorProduct");
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
     * Remove the VendorProduct
     */
    public static Map<String, Object> deleteVendorProduct(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookedUpValue = delegator.makeValue("VendorProduct");
        lookedUpValue.setPKFields(context);
        try {
            delegator.removeValue(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Create a ProductCategoryGlAccount
     */
    public static Map<String, Object> createProductCategoryGlAccount(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Map<String, Object> lookupRoleCategoriesMap = null;
        Object checkAction = null;
        List<GenericValue> emptyField = null;
        String callingMethodName = null;
        Object checkActionLabel = null;
        Object resourceDescription = null;
        List<GenericValue> roleCategories = null;
        callingMethodName = "createProductCategoryGlAccount";
        checkAction = "CREATE";
        Map<String, Object> inlineResult = checkProductRelatedPermission(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue newEntity = delegator.makeValue("ProductCategoryGlAccount");
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
     * Update a ProductCategoryGlAccount
     */
    public static Map<String, Object> updateProductCategoryGlAccount(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Map<String, Object> lookupRoleCategoriesMap = null;
        Object checkAction = null;
        List<GenericValue> emptyField = null;
        String callingMethodName = null;
        Object checkActionLabel = null;
        Object resourceDescription = null;
        List<GenericValue> roleCategories = null;
        callingMethodName = "updateProductCategoryGlAccount";
        checkAction = "UPDATE";
        Map<String, Object> inlineResult = checkProductRelatedPermission(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ProductCategoryGlAccount")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ProductCategoryGlAccount: " + e.getMessage(), MODULE);
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
     * Delete a ProductCategoryGlAccount
     */
    public static Map<String, Object> deleteProductCategoryGlAccount(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Map<String, Object> lookupRoleCategoriesMap = null;
        Object checkAction = null;
        List<GenericValue> emptyField = null;
        String callingMethodName = null;
        Object checkActionLabel = null;
        Object resourceDescription = null;
        List<GenericValue> roleCategories = null;
        callingMethodName = "deleteProductCategoryGlAccount";
        checkAction = "DELETE";
        Map<String, Object> inlineResult = checkProductRelatedPermission(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ProductCategoryGlAccount")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ProductCategoryGlAccount: " + e.getMessage(), MODULE);
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
     * Create ProductGroupOrder
     */
    public static Map<String, Object> createProductGroupOrder(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue newEntity = delegator.makeValue("ProductGroupOrder");
        delegator.setNextSubSeqId(newEntity, "groupOrderId", 5, 1);
        Object groupOrderId = newEntity.get("groupOrderId");
        result.put("groupOrderId", newEntity.get("groupOrderId"));
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
     * Update ProductGroupOrder
     */
    public static Map<String, Object> updateProductGroupOrder(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue jobSandbox = null;
        GenericValue productGroupOrder = null;
        try {
            productGroupOrder = EntityQuery.use(delegator)
                    .from("ProductGroupOrder")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ProductGroupOrder: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        productGroupOrder.setNonPKFields(context);
        try {
            delegator.store(productGroupOrder);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if ("GO_CREATED".equals(productGroupOrder.get("statusId"))) {
            try {
                jobSandbox = EntityQuery.use(delegator)
                        .from("JobSandbox")
                        .where(UtilMisc.toMap("jobId", productGroupOrder.get("jobId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying JobSandbox: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (UtilValidate.isNotEmpty(jobSandbox)) {
                jobSandbox.put("runTime", context.get("thruDate"));
                try {
                    delegator.store(jobSandbox);
                } catch (Exception e) {
                    Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }

        return result;
    }


    /**
     * Delete ProductGroupOrder
     */
    public static Map<String, Object> deleteProductGroupOrder(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<GenericValue> orderItemGroupOrders = null;
        try {
            orderItemGroupOrders = EntityQuery.use(delegator)
                    .from("OrderItemGroupOrder")
                    .where(UtilMisc.toMap("groupOrderId", context.get("groupOrderId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (orderItemGroupOrders != null) {
            for (GenericValue orderItemGroupOrder : orderItemGroupOrders) {
                try {
                    delegator.removeValue(orderItemGroupOrder);
                } catch (Exception e) {
                    Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }
        GenericValue productGroupOrder = null;
        try {
            productGroupOrder = EntityQuery.use(delegator)
                    .from("ProductGroupOrder")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ProductGroupOrder: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            delegator.removeValue(productGroupOrder);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue jobSandbox = null;
        try {
            jobSandbox = EntityQuery.use(delegator)
                    .from("JobSandbox")
                    .where(UtilMisc.toMap("jobId", productGroupOrder.get("jobId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying JobSandbox: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            delegator.removeValue(jobSandbox);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        List<GenericValue> jobSandboxList = null;
        try {
            jobSandboxList = EntityQuery.use(delegator)
                    .from("JobSandbox")
                    .where(UtilMisc.toMap("runtimeDataId", jobSandbox.get("runtimeDataId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (jobSandboxList != null) {
            for (GenericValue jobSandboxRelatedRuntimeData : jobSandboxList) {
                try {
                    delegator.removeValue(jobSandboxRelatedRuntimeData);
                } catch (Exception e) {
                    Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }
        GenericValue runtimeData = null;
        try {
            runtimeData = EntityQuery.use(delegator)
                    .from("RuntimeData")
                    .where(UtilMisc.toMap("runtimeDataId", jobSandbox.get("runtimeDataId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying RuntimeData: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            delegator.removeValue(runtimeData);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Create ProductGroupOrder
     */
    public static Map<String, Object> createJobForProductGroupOrder(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Object jobId = null;
        GenericValue runtimeData = null;
        Map<String, Object> runtimeDataMap = null;
        GenericValue jobSandbox = null;
        GenericValue productGroupOrder = null;
        Object runtimeInfo = null;
        Object runtimeDataId = null;
        try {
            productGroupOrder = EntityQuery.use(delegator)
                    .from("ProductGroupOrder")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ProductGroupOrder: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(productGroupOrder.get("jobId"))) {
            runtimeDataMap.put("groupOrderId", context.get("groupOrderId"));
            try {
                runtimeInfo = XmlSerializer.serialize(runtimeDataMap);
            } catch (Exception e) {
                Debug.logError(e, "Error calling XmlSerializer.serialize: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            runtimeData = delegator.makeValue("RuntimeData");
            ((GenericValue) runtimeData).put("runtimeDataId", delegator.getNextSeqId("RuntimeData"));
            runtimeDataId = runtimeData.get("runtimeDataId");
            runtimeData.put("runtimeInfo", runtimeInfo);
            try {
                delegator.create(runtimeData);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            jobSandbox = delegator.makeValue("JobSandbox");
            ((GenericValue) jobSandbox).put("jobId", delegator.getNextSeqId("JobSandbox"));
            jobId = jobSandbox.get("jobId");
            jobSandbox.put("jobName", "Check ProductGroupOrder Expired");
            jobSandbox.put("runTime", context.get("thruDate"));
            jobSandbox.put("poolId", "pool");
            jobSandbox.put("statusId", "SERVICE_PENDING");
            jobSandbox.put("serviceName", "checkProductGroupOrderExpired");
            jobSandbox.put("runAsUser", "system");
            jobSandbox.put("runtimeDataId", runtimeDataId);
            jobSandbox.put("maxRecurrenceCount", 1L);
            jobSandbox.put("priority", 50L);
            try {
                delegator.create(jobSandbox);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            productGroupOrder.put("jobId", jobId);
            try {
                delegator.store(productGroupOrder);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        return result;
    }


    /**
     * Check OrderItem For ProductGroupOrder
     */
    public static Map<String, Object> checkOrderItemForProductGroupOrder(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue product = null;
        Map<String, Object> createOrderItemGroupOrderMap = null;
        Object productId = null;
        List<GenericValue> variantProductAssocs = null;
        List<GenericValue> productGroupOrders = null;
        GenericValue productGroupOrder = null;
        GenericValue variantProductAssoc = null;
        List<GenericValue> orderItems = null;
        try {
            orderItems = EntityQuery.use(delegator)
                    .from("OrderItem")
                    .where(UtilMisc.toMap("orderId", context.get("orderId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (orderItems != null) {
            for (GenericValue orderItem : orderItems) {
                productId = orderItem.get("productId");
                try {
                    product = EntityQuery.use(delegator)
                            .from("Product")
                            .where(UtilMisc.toMap("productId", orderItem.get("productId")))
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying Product: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if ("Y".equals(product.get("isVariant"))) {
                    try {
                        variantProductAssocs = EntityQuery.use(delegator)
                                .from("ProductAssoc")
                                .where(UtilMisc.toMap("productIdTo", orderItem.get("productId"), "productAssocTypeId", "PRODUCT_VARIANT"))
                                .filterByDate()
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    variantProductAssoc = EntityUtil.getFirst((List<GenericValue>) variantProductAssocs);
                    productId = variantProductAssoc.get("productId");
                }
                try {
                    productGroupOrders = EntityQuery.use(delegator)
                            .from("ProductGroupOrder")
                            .where(UtilMisc.toMap("productId", productId))
                            .filterByDate()
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (UtilValidate.isNotEmpty(productGroupOrders)) {
                    productGroupOrder = EntityUtil.getFirst((List<GenericValue>) productGroupOrders);
                    productGroupOrder.set("soldOrderQty", new BigDecimal(orderItem.get("quantity").toString()));
                    try {
                        delegator.store(productGroupOrder);
                    } catch (Exception e) {
                        Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    createOrderItemGroupOrderMap.put("orderId", orderItem.get("orderId"));
                    createOrderItemGroupOrderMap.put("orderItemSeqId", orderItem.get("orderItemSeqId"));
                    createOrderItemGroupOrderMap.put("groupOrderId", productGroupOrder.get("groupOrderId"));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("createOrderItemGroupOrder", createOrderItemGroupOrderMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling createOrderItemGroupOrder: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            }
        }

        return result;
    }


    /**
     * Cancle OrderItemGroupOrder
     */
    public static Map<String, Object> cancleOrderItemGroupOrder(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<GenericValue> orderItems = null;
        GenericValue orderItemGroupOrder = null;
        GenericValue productGroupOrder = null;
        Object cancelQuantity = null;
        List<GenericValue> orderItemGroupOrders = null;
        if (UtilValidate.isNotEmpty(context.get("orderItemSeqId"))) {
            try {
                orderItems = EntityQuery.use(delegator)
                        .from("OrderItem")
                        .where(UtilMisc.toMap("orderId", context.get("orderId"), "orderItemSeqId", context.get("orderItemSeqId")))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        } else {
            try {
                orderItems = EntityQuery.use(delegator)
                        .from("OrderItem")
                        .where(UtilMisc.toMap("orderId", context.get("orderId")))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        if (orderItems != null) {
            for (GenericValue orderItem : orderItems) {
                try {
                    orderItemGroupOrders = EntityQuery.use(delegator)
                            .from("OrderItemGroupOrder")
                            .where(UtilMisc.toMap("orderId", orderItem.get("orderId"), "orderItemSeqId", orderItem.get("orderItemSeqId")))
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (UtilValidate.isNotEmpty(orderItemGroupOrders)) {
                    orderItemGroupOrder = EntityUtil.getFirst((List<GenericValue>) orderItemGroupOrders);
                    try {
                        productGroupOrder = EntityQuery.use(delegator)
                                .from("ProductGroupOrder")
                                .where(UtilMisc.toMap("groupOrderId", orderItemGroupOrder.get("groupOrderId")))
                                .queryOne();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying ProductGroupOrder: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    if (UtilValidate.isNotEmpty(productGroupOrder)) {
                        if ("GO_CREATED".equals(productGroupOrder.get("statusId"))) {
                            if ("ITEM_CANCELLED".equals(orderItem.get("statusId"))) {
                                if (UtilValidate.isNotEmpty(orderItem.get("cancelQuantity"))) {
                                    cancelQuantity = orderItem.get("cancelQuantity");
                                } else {
                                    cancelQuantity = orderItem.get("quantity");
                                }
                                productGroupOrder.set("soldOrderQty", new BigDecimal(cancelQuantity.toString()));
                            }
                            try {
                                delegator.store(productGroupOrder);
                            } catch (Exception e) {
                                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                                return ServiceUtil.returnError(e.getMessage());
                            }
                            try {
                                delegator.removeValue(orderItemGroupOrder);
                            } catch (Exception e) {
                                Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                                return ServiceUtil.returnError(e.getMessage());
                            }
                        }
                    }
                }
            }
        }

        return result;
    }


    /**
     * Check ProductGroupOrder Expired
     */
    public static Map<String, Object> checkProductGroupOrderExpired(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Object groupOrderStatusId = null;
        Map<String, Object> changeOrderItemStatusMap = null;
        Object newItemStatusId = null;
        Map<String, Object> updateProductGroupOrderMap = null;
        List<GenericValue> orderItemGroupOrders = null;
        GenericValue productGroupOrder = null;
        try {
            productGroupOrder = EntityQuery.use(delegator)
                    .from("ProductGroupOrder")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ProductGroupOrder: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(productGroupOrder)) {
            if (((Comparable) productGroupOrder.get("soldOrderQty")).compareTo(productGroupOrder.get("reqOrderQty")) >= 0) {
                newItemStatusId = "ITEM_APPROVED";
                groupOrderStatusId = "GO_SUCCESS";
            } else {
                newItemStatusId = "ITEM_CANCELLED";
                groupOrderStatusId = "GO_CANCELLED";
            }
            updateProductGroupOrderMap.put("groupOrderId", productGroupOrder.get("groupOrderId"));
            updateProductGroupOrderMap.put("statusId", groupOrderStatusId);
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updateProductGroupOrder", updateProductGroupOrderMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling updateProductGroupOrder: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            try {
                orderItemGroupOrders = EntityQuery.use(delegator)
                        .from("OrderItemGroupOrder")
                        .where(UtilMisc.toMap("groupOrderId", productGroupOrder.get("groupOrderId")))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (orderItemGroupOrders != null) {
                for (GenericValue orderItemGroupOrder : orderItemGroupOrders) {
                    changeOrderItemStatusMap.put("orderId", orderItemGroupOrder.get("orderId"));
                    changeOrderItemStatusMap.put("orderItemSeqId", orderItemGroupOrder.get("orderItemSeqId"));
                    changeOrderItemStatusMap.put("statusId", newItemStatusId);
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("changeOrderItemStatus", changeOrderItemStatusMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling changeOrderItemStatus: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            }
        }

        return result;
    }


    /**
     * change the product review Status
     */
    public static Map<String, Object> setProductReviewStatus(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Object msg = null;
        GenericValue statusChange = null;
        Map<String, Object> lookupRoleCategoriesMap = null;
        Object checkAction = null;
        List<GenericValue> emptyField = null;
        String callingMethodName = null;
        Object checkActionLabel = null;
        Object resourceDescription = null;
        List<GenericValue> roleCategories = null;
        callingMethodName = "setProductReviewStatus";
        checkAction = "UPDATE";
        Map<String, Object> inlineResult = checkProductRelatedPermission(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue productReview = null;
        try {
            productReview = EntityQuery.use(delegator)
                    .from("ProductReview")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ProductReview: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(productReview)) {
            if (!java.util.Objects.equals(productReview.get("statusId"), context.get("statusId"))) {
                try {
                    statusChange = EntityQuery.use(delegator)
                            .from("StatusValidChange")
                            .where(UtilMisc.toMap("statusId", productReview.get("statusId"), "statusIdTo", context.get("statusId")))
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying StatusValidChange: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (UtilValidate.isEmpty(statusChange)) {
                    msg = "Status is not a valid change: from " + productReview.get("statusId") + " to " + context.get("statusId");
                    Debug.logError(String.valueOf(msg), MODULE);
                    {
                        String errorMsg = UtilProperties.getMessage("ProductErrorUiLabels", "ProductReviewErrorCouldNotChangeOrderStatusFromTo", locale);
                        error_list.add(errorMsg);
                    }
                }
            }
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        productReview.put("statusId", context.get("statusId"));
        try {
            delegator.store(productReview);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        result.put("productReviewId", productReview.get("productReviewId"));

        return result;
    }


    /**
     */
    public static Map<String, Object> createProductAndCatAssocVersatile(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> updateLocTextCtx = new HashMap<>();
        Map<String, Object> createRecordCtx = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "createRecordCtx" for service "createProduct"
        createRecordCtx.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createProduct", createRecordCtx);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            context.put("productId", serviceResult.get("productId"));
            result.put("productId", serviceResult.get("productId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createProduct: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> createAssocCtx = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "createAssocCtx" for service "addProductToCategory"
        createAssocCtx.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("addProductToCategory", createAssocCtx);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            result.put("productCategoryId", serviceResult.get("productCategoryId"));
            result.put("fromDate", serviceResult.get("fromDate"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling addProductToCategory: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (Boolean.TRUE.equals(context.get("updateLocalizedTexts"))) {
            // set-service-fields from "parameters" to "updateLocTextCtx" for service "replaceProductContentLocalizedSimpleTexts"
            updateLocTextCtx.putAll(UtilMisc.toMap(context));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("replaceProductContentLocalizedSimpleTexts", updateLocTextCtx);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling replaceProductContentLocalizedSimpleTexts: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        return result;
    }


    /**
     */
    public static Map<String, Object> updateProductAndCatAssocVersatile(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> updateLocTextCtx = new HashMap<>();
        Map<String, Object> updateRecordCtx = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "updateRecordCtx" for service "updateProduct"
        updateRecordCtx.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateProduct", updateRecordCtx);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateProduct: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> updateAssocCtx = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "updateAssocCtx" for service "updateProductToCategory"
        updateAssocCtx.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateProductToCategory", updateAssocCtx);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateProductToCategory: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (Boolean.TRUE.equals(context.get("updateLocalizedTexts"))) {
            // set-service-fields from "parameters" to "updateLocTextCtx" for service "replaceProductContentLocalizedSimpleTexts"
            updateLocTextCtx.putAll(UtilMisc.toMap(context));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("replaceProductContentLocalizedSimpleTexts", updateLocTextCtx);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling replaceProductContentLocalizedSimpleTexts: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        return result;
    }


    /**
     */
    public static Map<String, Object> deleteProductCatAssocVersatile(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> removeCtx = new HashMap<>();
        Map<String, Object> updateCtx = new HashMap<>();
        if ("expire".equals(context.get("deleteAssocMode"))) {
            // set-service-fields from "parameters" to "updateCtx" for service "updateProductToCategory"
            updateCtx.putAll(UtilMisc.toMap(context));
            Timestamp updateCtx_thruDate = new Timestamp(System.currentTimeMillis());
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updateProductToCategory", updateCtx);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling updateProductToCategory: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        } else {
            // set-service-fields from "parameters" to "removeCtx" for service "removeProductFromCategory"
            removeCtx.putAll(UtilMisc.toMap(context));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("removeProductFromCategory", removeCtx);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling removeProductFromCategory: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        return result;
    }


    /**
     */
    public static Map<String, Object> deleteProductAndRelatedVersatile(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        String errMsgStr = null;
        Object assocProdFilterByDate = null;
        Map<String, Object> productAssocRemoveCtx = new HashMap<>();
        List<GenericValue> assocProductRecList = null;
        Object assocContentFilterByDate = null;
        Object contentRemoveCtx = null;
        List<GenericValue> recursiveContentList = null;
        List<GenericValue> values = null;
        Map<String, Object> lookupRoleCategoriesMap = null;
        Object checkAction = null;
        List<GenericValue> emptyField = null;
        String callingMethodName = null;
        Object checkActionLabel = null;
        Object resourceDescription = null;
        List<GenericValue> roleCategories = null;
        callingMethodName = "deleteProductAndRelatedVersatile";
        checkAction = "DELETE";
        Map<String, Object> inlineResult = checkProductRelatedPermission(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
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
        if (UtilValidate.isEmpty(product)) {
            errMsgStr = UtilProperties.getMessage("ProductUiLabels", "ProductProductNotFound", locale);
            error_list.add("${errMsgStr}: ${parameters.productId}");
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        }
        Map<String, Object> prodIdMap = new HashMap<String, Object>();
        prodIdMap.put("productId", context.get("productId"));
        assocProdFilterByDate = null;
        if ("all".equals(context.get("deleteAssocProductRecursive"))) {
            assocProdFilterByDate = "false";
        } else {
            if ("active".equals(context.get("deleteAssocProductRecursive"))) {
                assocProdFilterByDate = "true";
            }
        }
        if (UtilValidate.isNotEmpty(assocProdFilterByDate)) {
            try {
                assocProductRecList = EntityQuery.use(delegator)
                        .from("ProductAssoc")
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ProductAssoc: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (context.get("productAssocList") != null) {
                for (Object productAssoc : (List<?>) context.get("productAssocList")) {
                    productAssocRemoveCtx = new HashMap<String, Object>();
                    // set-service-fields from "parameters" to "productAssocRemoveCtx" for service "deleteProductAndRelatedVersatile"
                    productAssocRemoveCtx.putAll(UtilMisc.toMap(context));
                    productAssocRemoveCtx.put("productId", ((Map<String, Object>) productAssoc).get("productId"));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("deleteProductAndRelatedVersatile", productAssocRemoveCtx);
                        if (ServiceUtil.isError(serviceResult)) {
                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling deleteProductAndRelatedVersatile: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            }
        }
        try {
            delegator.removeByAnd("ProductAssoc", prodIdMap);
        } catch (Exception e) {
            Debug.logError(e, "Error removing ProductAssoc: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> prodIdToMap = new HashMap<String, Object>();
        prodIdToMap.put("productIdTo", context.get("productId"));
        try {
            delegator.removeByAnd("ProductAssoc", prodIdToMap);
        } catch (Exception e) {
            Debug.logError(e, "Error removing ProductAssoc: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            delegator.removeByAnd("ProductAttribute", prodIdMap);
        } catch (Exception e) {
            Debug.logError(e, "Error removing ProductAttribute: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            delegator.removeByAnd("ProductKeyword", prodIdMap);
        } catch (Exception e) {
            Debug.logError(e, "Error removing ProductKeyword: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            delegator.removeByAnd("ProductRole", prodIdMap);
        } catch (Exception e) {
            Debug.logError(e, "Error removing ProductRole: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            delegator.removeByAnd("ProductCalculatedInfo", prodIdMap);
        } catch (Exception e) {
            Debug.logError(e, "Error removing ProductCalculatedInfo: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            delegator.removeByAnd("ProductGeo", prodIdMap);
        } catch (Exception e) {
            Debug.logError(e, "Error removing ProductGeo: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            delegator.removeByAnd("ProductPriceChange", prodIdMap);
        } catch (Exception e) {
            Debug.logError(e, "Error removing ProductPriceChange: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            delegator.removeByAnd("ProductPrice", prodIdMap);
        } catch (Exception e) {
            Debug.logError(e, "Error removing ProductPrice: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            delegator.removeByAnd("ProductConfig", prodIdMap);
        } catch (Exception e) {
            Debug.logError(e, "Error removing ProductConfig: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            delegator.removeByAnd("ProductConfigProduct", prodIdMap);
        } catch (Exception e) {
            Debug.logError(e, "Error removing ProductConfigProduct: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            delegator.removeByAnd("ProductConfigStats", prodIdMap);
        } catch (Exception e) {
            Debug.logError(e, "Error removing ProductConfigStats: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            delegator.removeByAnd("ConfigOptionProductOption", prodIdMap);
        } catch (Exception e) {
            Debug.logError(e, "Error removing ConfigOptionProductOption: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            delegator.removeByAnd("GoodIdentification", prodIdMap);
        } catch (Exception e) {
            Debug.logError(e, "Error removing GoodIdentification: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            delegator.removeByAnd("ProductPaymentMethodType", prodIdMap);
        } catch (Exception e) {
            Debug.logError(e, "Error removing ProductPaymentMethodType: " + e.getMessage(), MODULE);
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
                        .from("ProductContent")
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ProductContent: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (recursiveContentList != null) {
                for (GenericValue productContent : recursiveContentList) {
                    contentRemoveCtx = null;
                    ((Map<String, Object>) contentRemoveCtx).put("contentId", productContent.get("contentId"));
                    ((Map<String, Object>) contentRemoveCtx).put("recursiveTarget", context.get("deleteContentRecursive"));
                    try {
                        delegator.removeValue(productContent);
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
            delegator.removeByAnd("ProductContent", prodIdMap);
        } catch (Exception e) {
            Debug.logError(e, "Error removing ProductContent: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if ("expired".equals(context.get("deleteParentAssocSelect"))) {
            try {
                values = EntityQuery.use(delegator)
                        .from("ProductCategoryMember")
                        .where(UtilMisc.toMap("productId", ((Map<String, Object>) prodIdMap).get("productId")))
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
                delegator.removeByAnd("ProductCategoryMember", prodIdMap);
            } catch (Exception e) {
                Debug.logError(e, "Error removing ProductCategoryMember: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        if ("recursive".equals(context.get("deleteChildrenSelect"))) {
        }
        if ("expired".equals(context.get("deleteChildAssocSelect"))) {
        }
        if ("all".equals(context.get("deleteChildAssocSelect"))) {
        }
        if ("expired".equals(context.get("deleteSpecialAssocSelect"))) {
        }
        if ("all".equals(context.get("deleteSpecialAssocSelect"))) {
        }
        try {
            delegator.removeValue(product);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     */
    public static Map<String, Object> deleteProductAndCatAssocVersatile(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> deleteRecordCtx = new HashMap<>();
        Map<String, Object> deleteAssocCtx = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "deleteAssocCtx" for service "deleteProductCatAssocVersatile"
        deleteAssocCtx.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("deleteProductCatAssocVersatile", deleteAssocCtx);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling deleteProductCatAssocVersatile: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (!Boolean.FALSE.equals(context.get("deleteRecordAndRelated"))) {
            // set-service-fields from "parameters" to "deleteRecordCtx" for service "deleteProductAndRelatedVersatile"
            deleteRecordCtx.putAll(UtilMisc.toMap(context));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("deleteProductAndRelatedVersatile", deleteRecordCtx);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling deleteProductAndRelatedVersatile: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        return result;
    }


    /**
     */
    public static Map<String, Object> addProductCatAssocVersatile(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        List<GenericValue> assocList = null;
        try {
            assocList = EntityQuery.use(delegator)
                    .from("ProductCategoryMember")
                    .where(UtilMisc.toMap("productCategoryId", context.get("productCategoryId"), "productId", context.get("productId")))
                    .filterByDate()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(assocList)) {
            {
                String errorMsg = UtilProperties.getMessage("ProductErrorUiLabels", "productservices.could_not_create_product_category_assoc_association_exists", locale);
                error_list.add(errorMsg);
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        }
        Map<String, Object> createAssocCtx = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "createAssocCtx" for service "addProductToCategory"
        createAssocCtx.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("addProductToCategory", createAssocCtx);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            result.put("productCategoryId", serviceResult.get("productCategoryId"));
            result.put("productId", serviceResult.get("productId"));
            result.put("fromDate", serviceResult.get("fromDate"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling addProductToCategory: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     */
    public static Map<String, Object> copyProductCatAssocVersatile(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> addAssocCtx = new HashMap<String, Object>();
        addAssocCtx.put("productId", context.get("productId"));
        addAssocCtx.put("fromDate", context.get("to_fromDate"));
        addAssocCtx.put("sequenceNum", context.get("to_sequenceNum"));
        addAssocCtx.put("productCategoryId", context.get("to_productCategoryId"));
        Map<String, Object> resultFields = new HashMap<String, Object>();
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("addProductCatAssocVersatile", addAssocCtx);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            resultFields.put("productId", serviceResult.get("productId"));
            resultFields.put("fromDate", serviceResult.get("fromDate"));
            resultFields.put("sequenceNum", serviceResult.get("sequenceNum"));
            resultFields.put("productCategoryId", serviceResult.get("productCategoryId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling addProductCatAssocVersatile: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        result.put("productId", ((Map<String, Object>) resultFields).get("productId"));
        if (Boolean.TRUE.equals(context.get("returnAssocFields"))) {
            result.put("fromDate", ((Map<String, Object>) resultFields).get("fromDate"));
            result.put("sequenceNum", ((Map<String, Object>) resultFields).get("sequenceNum"));
            result.put("productCategoryId", ((Map<String, Object>) resultFields).get("productCategoryId"));
        }
        result.put("to_productId", ((Map<String, Object>) resultFields).get("productId"));
        result.put("to_fromDate", ((Map<String, Object>) resultFields).get("fromDate"));
        result.put("to_sequenceNum", ((Map<String, Object>) resultFields).get("sequenceNum"));
        result.put("to_productCategoryId", ((Map<String, Object>) resultFields).get("productCategoryId"));

        return result;
    }


    /**
     */
    public static Map<String, Object> moveProductCatAssocVersatile(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Map<String, Object> addAssocCtx = new HashMap<>();
        Map<String, Object> resultFields = null;
        Map<String, Object> deleteAssocCtx = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "deleteAssocCtx" for service "deleteProductCatAssocVersatile"
        deleteAssocCtx.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("deleteProductCatAssocVersatile", deleteAssocCtx);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling deleteProductCatAssocVersatile: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> inlineResult = copyProductCatAssocVersatile(dctx, context);
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
    public static Map<String, Object> getProductExtendedDataVersatile(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        String errMsgStr = null;
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
        result.put("product", product);
        if (UtilValidate.isEmpty(product)) {
            errMsgStr = UtilProperties.getMessage("ProductUiLabels", "ProductProductNotFound", locale);
            error_list.add("${errMsgStr}: ${parameters.productId}");
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        }
        Map<String, Object> locTextCtx = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "locTextCtx" for service "getProductContentLocalizedSimpleTextViews"
        locTextCtx.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("getProductContentLocalizedSimpleTextViews", locTextCtx);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            result.put("viewsByType", serviceResult.get("viewsByType"));
            result.put("viewsByTypeAndLocale", serviceResult.get("viewsByTypeAndLocale"));
            result.put("textByTypeAndLocale", serviceResult.get("textByTypeAndLocale"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling getProductContentLocalizedSimpleTextViews: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }

}

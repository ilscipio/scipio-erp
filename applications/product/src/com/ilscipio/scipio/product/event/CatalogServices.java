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
import org.ofbiz.base.util.UtilURL;
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
 * <p>Generated from: component://product/script/org/ofbiz/product/catalog/CatalogServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class CatalogServices {

    private static final String MODULE = CatalogServices.class.getName();


    /**
     * get All categories
     */
    public static Map<String, Object> getAllCategories(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        String defaultTopCategoryId = null;
        if (UtilValidate.isNotEmpty(context.get("topCategory"))) {
            defaultTopCategoryId = (String) context.get("topCategory");
        } else {
            defaultTopCategoryId = UtilProperties.getMessage("catalog", "top.category.default", locale);
        }
        Map<String, Object> relatedCategoryContext = new HashMap<String, Object>();
        relatedCategoryContext.put("parentProductCategoryId", defaultTopCategoryId);
        Object resCategories = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("getRelatedCategories", relatedCategoryContext);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            resCategories = serviceResult.get("categories");
        } catch (Exception e) {
            Debug.logError(e, "Error calling getRelatedCategories: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        result.put("categories", resCategories);

        return result;
    }


    /**
     * get All Related categories
     */
    public static Map<String, Object> getRelatedCategories(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<Object> categories = null;
        Map<String, Object> relatedCategoryContext = null;
        Object relCategories = null;
        Object addInCategories = null;
        GenericValue currentProductCategory = null;
        List<Object> subCategories = null;
        Map<String, Object> productCategoryContext = null;
        GenericValue productCategory = null;
        Object orderByString = "sequenceNum";
        List<Object> orderByStringList = new LinkedList<>();
        orderByStringList.add(orderByString);
        Map<String, Object> productCategoryRollUpContext = new HashMap<String, Object>();
        productCategoryRollUpContext.put("parentProductCategoryId", context.get("parentProductCategoryId"));
        List<GenericValue> rollups = null;
        try {
            rollups = EntityQuery.use(delegator)
                    .from("ProductCategoryRollup")
                    .where(productCategoryRollUpContext)
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ProductCategoryRollup: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(context.get("categories"))) {
            categories.addAll(UtilGenerics.cast(context.get("categories")));
        }
        GenericValue parent = null;
        Object subCategory = null;
        GenericValue relCategory = null;
        if (UtilValidate.isNotEmpty(rollups)) {
            if (rollups != null) {
                for (GenericValue parentEntry : rollups) {
                    try {
                        currentProductCategory = parentEntry.getRelatedOne("CurrentProductCategory", false);
                    } catch (Exception e) {
                        Debug.logError(e, "Error getting related one CurrentProductCategory: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    subCategories.add(currentProductCategory);
                }
            }
            if (UtilValidate.isNotEmpty(subCategories)) {
                relatedCategoryContext.put("categories", subCategories);
                if (subCategories != null) {
                    for (Object subCategoryEntry : subCategories) {
                        relatedCategoryContext.put("parentProductCategoryId", ((Map<String, Object>) subCategoryEntry).get("productCategoryId"));
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("getRelatedCategories", relatedCategoryContext);
                            if (ServiceUtil.isError(serviceResult)) {
                                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                            }
                            relCategories = serviceResult.get("categories");
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling getRelatedCategories: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                        if (UtilValidate.isNotEmpty(relCategories)) {
                            if (UtilValidate.isNotEmpty(categories)) {
                                if (relCategories != null) {
                                    for (Object relCategoryEntry : (List<?>) relCategories) {
                                        addInCategories = categories.contains(relCategoryEntry);
                                        if (Boolean.FALSE.equals(addInCategories)) {
                                            categories.add(relCategoryEntry);
                                        }
                                    }
                                }
                            } else {
                                categories.addAll(UtilGenerics.cast(relCategories));
                            }
                            result.put("categories", categories);
                        }
                    }
                }
            }
        } else {
            productCategoryContext.put("productCategoryId", context.get("parentProductCategoryId"));
            try {
                productCategory = EntityQuery.use(delegator)
                        .from("ProductCategory")
                        .where(productCategoryContext)
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error finding by primary key ProductCategory: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            categories.add(productCategory);
            result.put("categories", categories);
        }
        result.put("categories", categories);

        return result;
    }


    /**
     * Check for image url exists or not for All categories
     */
    public static Map<String, Object> checkImageUrlForAllCategories(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> categoriesMap = null;
        Map<String, Object> fileStatusMap = null;
        Map<String, Object> checkImageUrlForCategoryContext = null;
        Object categoryId = null;
        Map<String, Object> categoryFindContext = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "categoryFindContext" for service "getAllCategories"
        categoryFindContext.putAll(UtilMisc.toMap(context));
        Object categories = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("getAllCategories", categoryFindContext);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            categories = serviceResult.get("categories");
        } catch (Exception e) {
            Debug.logError(e, "Error calling getAllCategories: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(categories)) {
            if (categories != null) {
                for (Object category : (List<?>) categories) {
                    checkImageUrlForCategoryContext.put("categoryId", ((Map<String, Object>) category).get("productCategoryId"));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("checkImageUrlForCategoryAndProduct", checkImageUrlForCategoryContext);
                        if (ServiceUtil.isError(serviceResult)) {
                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                        }
                        fileStatusMap.put("fileExists", serviceResult.get("fileExists"));
                        fileStatusMap.put("fileNotExists", serviceResult.get("fileNotExists"));
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling checkImageUrlForCategoryAndProduct: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    categoryId = ((Map<String, Object>) category).get("productCategoryId");
                    categoriesMap.put((String) categoryId, fileStatusMap);
                }
            }
            result.put("categoriesMap", categoriesMap);
        }

        return result;
    }


    /**
     * Check for image url exists or not for category and product 
     */
    public static Map<String, Object> checkImageUrlForCategoryAndProduct(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<Object> fileNotExists = null;
        List<GenericValue> emptyField = null;
        Object filesImageMap = null;
        List<Object> fileExists = null;
        Map<String, Object> checkImageUrlForCategoryContext = null;
        GenericValue product = null;
        Map<String, Object> virtualProductContext = null;
        Map<String, Object> variantProductContext = null;
        List<GenericValue> variantProducts = null;
        Map<String, Object> checkImageUrlForProductContext = null;
        Map<String, Object> productCategoryContext = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "productCategoryContext" for service "getProductCategoryMembers"
        productCategoryContext.putAll(UtilMisc.toMap(context));
        Object categoryMembers = null;
        Object category = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("getProductCategoryMembers", productCategoryContext);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            categoryMembers = serviceResult.get("categoryMembers");
            category = serviceResult.get("category");
        } catch (Exception e) {
            Debug.logError(e, "Error calling getProductCategoryMembers: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(category)) {
            checkImageUrlForCategoryContext.put("categoryId", ((Map<String, Object>) category).get("productCategoryId"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("checkImageUrlForCategory", checkImageUrlForCategoryContext);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
                filesImageMap = serviceResult.get("filesImageMap");
            } catch (Exception e) {
                Debug.logError(e, "Error calling checkImageUrlForCategory: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (UtilValidate.isNotEmpty(((Map<String, Object>) ((Map<String, Object>) filesImageMap).get("categoryImageUrlMap")).get("categoryImageUrl"))) {
                if ("Y".equals(((Map<String, Object>) ((Map<String, Object>) filesImageMap).get("categoryImageUrlMap")).get("isExists"))) {
                    emptyField.add(null);
                    fileExists.add(((Map<String, Object>) ((Map<String, Object>) filesImageMap).get("categoryImageUrlMap")).get("categoryImageUrl"));
                } else {
                    fileNotExists.add(((Map<String, Object>) ((Map<String, Object>) filesImageMap).get("categoryImageUrlMap")).get("categoryImageUrl"));
                }
            }
            if (UtilValidate.isNotEmpty(((Map<String, Object>) ((Map<String, Object>) filesImageMap).get("linkOneImageUrlMap")).get("linkOneImageUrl"))) {
                if ("Y".equals(((Map<String, Object>) ((Map<String, Object>) filesImageMap).get("linkOneImageUrlMap")).get("isExists"))) {
                    fileExists.add(((Map<String, Object>) ((Map<String, Object>) filesImageMap).get("linkOneImageUrlMap")).get("linkOneImageUrl"));
                } else {
                    fileNotExists.add(((Map<String, Object>) ((Map<String, Object>) filesImageMap).get("linkOneImageUrlMap")).get("linkOneImageUrl"));
                }
            }
            if (UtilValidate.isNotEmpty(((Map<String, Object>) ((Map<String, Object>) filesImageMap).get("linkTwoImageUrlMap")).get("linkTwoImageUrl"))) {
                if ("Y".equals(((Map<String, Object>) ((Map<String, Object>) filesImageMap).get("linkTwoImageUrlMap")).get("isExists"))) {
                    fileExists.add(((Map<String, Object>) ((Map<String, Object>) filesImageMap).get("linkTwoImageUrlMap")).get("linkTwoImageUrl"));
                } else {
                    fileNotExists.add(((Map<String, Object>) ((Map<String, Object>) filesImageMap).get("linkTwoImageUrlMap")).get("linkTwoImageUrl"));
                }
            }
        }
        if (UtilValidate.isNotEmpty(categoryMembers)) {
            if (categoryMembers != null) {
                for (Object productCategoryMember : (List<?>) categoryMembers) {
                    checkImageUrlForProductContext.put("productId", ((Map<String, Object>) productCategoryMember).get("productId"));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("checkImageUrlForProduct", checkImageUrlForProductContext);
                        if (ServiceUtil.isError(serviceResult)) {
                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                        }
                        filesImageMap = serviceResult.get("filesImageMap");
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling checkImageUrlForProduct: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    if (UtilValidate.isNotEmpty(filesImageMap)) {
                        if (UtilValidate.isNotEmpty(((Map<String, Object>) ((Map<String, Object>) filesImageMap).get("smallImageUrlMap")).get("smallImageUrl"))) {
                            if ("Y".equals(((Map<String, Object>) ((Map<String, Object>) filesImageMap).get("smallImageUrlMap")).get("isExists"))) {
                                fileExists.add(((Map<String, Object>) ((Map<String, Object>) filesImageMap).get("smallImageUrlMap")).get("smallImageUrl"));
                            } else {
                                fileNotExists.add(((Map<String, Object>) ((Map<String, Object>) filesImageMap).get("smallImageUrlMap")).get("smallImageUrl"));
                            }
                        }
                        if (UtilValidate.isNotEmpty(((Map<String, Object>) ((Map<String, Object>) filesImageMap).get("mediumImageUrlMap")).get("mediumImageUrl"))) {
                            if ("Y".equals(((Map<String, Object>) ((Map<String, Object>) filesImageMap).get("mediumImageUrlMap")).get("isExists"))) {
                                fileExists.add(((Map<String, Object>) ((Map<String, Object>) filesImageMap).get("mediumImageUrlMap")).get("mediumImageUrl"));
                            } else {
                                fileNotExists.add(((Map<String, Object>) ((Map<String, Object>) filesImageMap).get("mediumImageUrlMap")).get("mediumImageUrl"));
                            }
                        }
                        if (UtilValidate.isNotEmpty(((Map<String, Object>) ((Map<String, Object>) filesImageMap).get("largeImageUrlMap")).get("largeImageUrl"))) {
                            if ("Y".equals(((Map<String, Object>) ((Map<String, Object>) filesImageMap).get("largeImageUrlMap")).get("isExists"))) {
                                fileExists.add(((Map<String, Object>) ((Map<String, Object>) filesImageMap).get("largeImageUrlMap")).get("largeImageUrl"));
                            } else {
                                fileNotExists.add(((Map<String, Object>) ((Map<String, Object>) filesImageMap).get("largeImageUrlMap")).get("largeImageUrl"));
                            }
                        }
                        if (UtilValidate.isNotEmpty(((Map<String, Object>) ((Map<String, Object>) filesImageMap).get("detailImageUrlMap")).get("detailImageUrl"))) {
                            if ("Y".equals(((Map<String, Object>) ((Map<String, Object>) filesImageMap).get("detailImageUrl")).get("isExists"))) {
                                fileExists.add(((Map<String, Object>) ((Map<String, Object>) filesImageMap).get("detailImageUrl")).get("detailImageUrl"));
                            } else {
                                fileNotExists.add(((Map<String, Object>) ((Map<String, Object>) filesImageMap).get("detailImageUrl")).get("detailImageUrl"));
                            }
                        }
                    }
                    try {
                        product = ((GenericValue) productCategoryMember).getRelatedOne("Product", false);
                    } catch (Exception e) {
                        Debug.logError(e, "Error getting related one Product: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    if ("Y".equals(product.get("isVirtual"))) {
                        virtualProductContext.put("productId", product.get("productId"));
                        virtualProductContext.put("productAssocTypeId", "PRODUCT_VARIANT");
                        try {
                            variantProducts = EntityQuery.use(delegator)
                                    .from("ProductAssoc")
                                    .where(virtualProductContext)
                                    .queryList();
                        } catch (Exception e) {
                            Debug.logError(e, "Error querying ProductAssoc: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                        emptyField = EntityUtil.filterByDate(UtilGenerics.cast(variantProducts));
                        if (UtilValidate.isNotEmpty(variantProducts)) {
                            if (variantProducts != null) {
                                for (GenericValue variantProduct : variantProducts) {
                                    variantProductContext.put("productId", variantProduct.get("productIdTo"));
                                    try {
                                        Map<String, Object> serviceResult = dispatcher.runSync("checkImageUrlForProduct", variantProductContext);
                                        if (ServiceUtil.isError(serviceResult)) {
                                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                                        }
                                        filesImageMap = serviceResult.get("filesImageMap");
                                    } catch (Exception e) {
                                        Debug.logError(e, "Error calling checkImageUrlForProduct: " + e.getMessage(), MODULE);
                                        return ServiceUtil.returnError(e.getMessage());
                                    }
                                    if (UtilValidate.isNotEmpty(filesImageMap)) {
                                        if (UtilValidate.isNotEmpty(((Map<String, Object>) ((Map<String, Object>) filesImageMap).get("smallImageUrlMap")).get("smallImageUrl"))) {
                                            if ("Y".equals(((Map<String, Object>) ((Map<String, Object>) filesImageMap).get("smallImageUrlMap")).get("isExists"))) {
                                                fileExists.add(((Map<String, Object>) ((Map<String, Object>) filesImageMap).get("smallImageUrlMap")).get("smallImageUrl"));
                                            } else {
                                                fileNotExists.add(((Map<String, Object>) ((Map<String, Object>) filesImageMap).get("smallImageUrlMap")).get("smallImageUrl"));
                                            }
                                        }
                                        if (UtilValidate.isNotEmpty(((Map<String, Object>) ((Map<String, Object>) filesImageMap).get("mediumImageUrlMap")).get("mediumImageUrl"))) {
                                            if ("Y".equals(((Map<String, Object>) ((Map<String, Object>) filesImageMap).get("mediumImageUrlMap")).get("isExists"))) {
                                                fileExists.add(((Map<String, Object>) ((Map<String, Object>) filesImageMap).get("mediumImageUrlMap")).get("mediumImageUrl"));
                                            } else {
                                                fileNotExists.add(((Map<String, Object>) ((Map<String, Object>) filesImageMap).get("mediumImageUrlMap")).get("mediumImageUrl"));
                                            }
                                        }
                                        if (UtilValidate.isNotEmpty(((Map<String, Object>) ((Map<String, Object>) filesImageMap).get("largeImageUrlMap")).get("largeImageUrl"))) {
                                            if ("Y".equals(((Map<String, Object>) ((Map<String, Object>) filesImageMap).get("largeImageUrlMap")).get("isExists"))) {
                                                fileExists.add(((Map<String, Object>) ((Map<String, Object>) filesImageMap).get("largeImageUrlMap")).get("largeImageUrl"));
                                            } else {
                                                fileNotExists.add(((Map<String, Object>) ((Map<String, Object>) filesImageMap).get("largeImageUrlMap")).get("largeImageUrl"));
                                            }
                                        }
                                        if (UtilValidate.isNotEmpty(((Map<String, Object>) ((Map<String, Object>) filesImageMap).get("detailImageUrlMap")).get("detailImageUrl"))) {
                                            if ("Y".equals(((Map<String, Object>) ((Map<String, Object>) filesImageMap).get("detailImageUrl")).get("isExists"))) {
                                                fileExists.add(((Map<String, Object>) ((Map<String, Object>) filesImageMap).get("detailImageUrl")).get("detailImageUrl"));
                                            } else {
                                                fileNotExists.add(((Map<String, Object>) ((Map<String, Object>) filesImageMap).get("detailImageUrl")).get("detailImageUrl"));
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            result.put("fileExists", fileExists);
            result.put("fileNotExists", fileNotExists);
        }

        return result;
    }


    /**
     * Check for image url exists or not for product
     */
    public static Map<String, Object> checkImageUrlForCategory(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> productCategoryFindContext = null;
        Object isExists = null;
        Map<String, Object> categoryImageUrlMap = null;
        Map<String, Object> linkOneImageUrlMap = null;
        Map<String, Object> filesImageMap = null;
        Map<String, Object> linkTwoImageUrlMap = null;
        Map<String, Object> checkImageUrlContext = null;
        GenericValue category = null;
        if (UtilValidate.isNotEmpty(context.get("categoryId"))) {
            productCategoryFindContext.put("productCategoryId", context.get("categoryId"));
            try {
                category = EntityQuery.use(delegator)
                        .from("ProductCategory")
                        .where(productCategoryFindContext)
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error finding by primary key ProductCategory: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (UtilValidate.isNotEmpty(category.get("categoryImageUrl"))) {
                checkImageUrlContext.put("imageUrl", category.get("categoryImageUrl"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("checkImageUrl", checkImageUrlContext);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                    isExists = serviceResult.get("isExists");
                } catch (Exception e) {
                    Debug.logError(e, "Error calling checkImageUrl: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                categoryImageUrlMap.put("categoryImageUrl", category.get("categoryImageUrl"));
                categoryImageUrlMap.put("isExists", isExists);
                filesImageMap.put("categoryImageUrlMap", categoryImageUrlMap);
                if ("N".equals(isExists)) {
                    category.remove("categoryImageUrl");
                }
            }
            if (UtilValidate.isNotEmpty(category.get("linkOneImageUrl"))) {
                checkImageUrlContext.remove("imageUrl");
                checkImageUrlContext.put("imageUrl", category.get("linkOneImageUrl"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("checkImageUrl", checkImageUrlContext);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                    isExists = serviceResult.get("isExists");
                } catch (Exception e) {
                    Debug.logError(e, "Error calling checkImageUrl: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                linkOneImageUrlMap.put("linkOneImageUrl", category.get("linkOneImageUrl"));
                linkOneImageUrlMap.put("isExists", isExists);
                filesImageMap.put("linkOneImageUrlMap", linkOneImageUrlMap);
                if ("N".equals(isExists)) {
                    category.remove("linkOneImageUrl");
                }
            }
            if (UtilValidate.isNotEmpty(category.get("linkTwoImageUrl"))) {
                checkImageUrlContext.remove("imageUrl");
                checkImageUrlContext.put("imageUrl", category.get("linkTwoImageUrl"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("checkImageUrl", checkImageUrlContext);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                    isExists = serviceResult.get("isExists");
                } catch (Exception e) {
                    Debug.logError(e, "Error calling checkImageUrl: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                linkTwoImageUrlMap.put("largeImageUrl", category.get("linkTwoImageUrl"));
                linkTwoImageUrlMap.put("isExists", isExists);
                filesImageMap.put("linkTwoImageUrlMap", linkTwoImageUrlMap);
                if ("N".equals(isExists)) {
                    category.remove("linkTwoImageUrl");
                }
            }
            try {
                delegator.store(category);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            result.put("filesImageMap", filesImageMap);
        }

        return result;
    }


    /**
     * Check for image url exists or not for product
     */
    public static Map<String, Object> checkImageUrlForProduct(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue product = null;
        Map<String, Object> smallImageUrlMap = null;
        Object isExists = null;
        Map<String, Object> largeImageUrlMap = null;
        Map<String, Object> filesImageMap = null;
        Map<String, Object> checkImageUrlContext = null;
        Map<String, Object> mediumImageUrlMap = null;
        Map<String, Object> productFindContext = null;
        Map<String, Object> detailImageUrlMap = null;
        if (UtilValidate.isNotEmpty(context.get("productId"))) {
            productFindContext.put("productId", context.get("productId"));
            try {
                product = EntityQuery.use(delegator)
                        .from("Product")
                        .where(productFindContext)
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error finding by primary key Product: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (UtilValidate.isNotEmpty(product.get("smallImageUrl"))) {
                checkImageUrlContext.put("imageUrl", product.get("smallImageUrl"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("checkImageUrl", checkImageUrlContext);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                    isExists = serviceResult.get("isExists");
                } catch (Exception e) {
                    Debug.logError(e, "Error calling checkImageUrl: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                smallImageUrlMap.put("smallImageUrl", product.get("smallImageUrl"));
                smallImageUrlMap.put("isExists", isExists);
                filesImageMap.put("smallImageUrlMap", smallImageUrlMap);
                if ("N".equals(isExists)) {
                    Debug.logInfo("Update SmallImage for product Id " + context.get("productId"), MODULE);
                    product.remove("smallImageUrl");
                }
            }
            if (UtilValidate.isNotEmpty(product.get("mediumImageUrl"))) {
                checkImageUrlContext.remove("imageUrl");
                checkImageUrlContext.put("imageUrl", product.get("mediumImageUrl"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("checkImageUrl", checkImageUrlContext);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                    isExists = serviceResult.get("isExists");
                } catch (Exception e) {
                    Debug.logError(e, "Error calling checkImageUrl: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                mediumImageUrlMap.put("mediumImageUrl", product.get("mediumImageUrl"));
                mediumImageUrlMap.put("isExists", isExists);
                filesImageMap.put("mediumImageUrlMap", mediumImageUrlMap);
                if ("N".equals(isExists)) {
                    product.remove("mediumImageUrl");
                }
            }
            if (UtilValidate.isNotEmpty(product.get("largeImageUrl"))) {
                checkImageUrlContext.remove("imageUrl");
                checkImageUrlContext.put("imageUrl", product.get("largeImageUrl"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("checkImageUrl", checkImageUrlContext);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                    isExists = serviceResult.get("isExists");
                } catch (Exception e) {
                    Debug.logError(e, "Error calling checkImageUrl: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                largeImageUrlMap.put("largeImageUrl", product.get("largeImageUrl"));
                largeImageUrlMap.put("isExists", isExists);
                filesImageMap.put("largeImageUrlMap", largeImageUrlMap);
                if ("N".equals(isExists)) {
                    product.remove("largeImageUrl");
                }
            }
            if (UtilValidate.isNotEmpty(product.get("detailImageUrl"))) {
                checkImageUrlContext.remove("imageUrl");
                checkImageUrlContext.put("imageUrl", product.get("detailImageUrl"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("checkImageUrl", checkImageUrlContext);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                    isExists = serviceResult.get("isExists");
                } catch (Exception e) {
                    Debug.logError(e, "Error calling checkImageUrl: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                detailImageUrlMap.put("detailImageUrl", product.get("detailImageUrl"));
                detailImageUrlMap.put("isExists", isExists);
                filesImageMap.put("detailImageUrlMap", detailImageUrlMap);
                if ("N".equals(isExists)) {
                    product.remove("detailImageUrl");
                }
            }
            result.put("filesImageMap", filesImageMap);
            try {
                delegator.store(product);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        return result;
    }


    /**
     * Check for image url exists or not
     */
    public static Map<String, Object> checkImageUrl(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Object imageUrl = null;
        Object url = null;
        Object isExists = null;
        imageUrl = context.get("imageUrl");
        boolean httpFlag = ((String) imageUrl).startsWith("http");
        boolean httpsFlag = ((String) imageUrl).startsWith("https");
        boolean ftpFlag = ((String) imageUrl).startsWith("ftp");
        if ((Boolean.TRUE.equals(httpFlag) || Boolean.TRUE.equals(httpsFlag) || Boolean.TRUE.equals(ftpFlag))) {
            try {
                url = UtilURL.fromUrlString("${imageUrl}");
            } catch (Exception e) {
                Debug.logError(e, "Error calling UtilURL.fromUrlString: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        } else {
            imageUrl = "/framework/images/webapp" + context.get("imageUrl");
            try {
                url = UtilURL.fromOfbizHomePath("${imageUrl}");
            } catch (Exception e) {
                Debug.logError(e, "Error calling UtilURL.fromOfbizHomePath: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        if (UtilValidate.isNotEmpty(url)) {
            isExists = "Y";
        } else {
            isExists = "N";
        }
        result.put("isExists", isExists);

        return result;
    }


    /**
     * Catalog permission logic
     */
    public static Map<String, Object> catalogPermissionCheck(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Security security = dctx.getSecurity();
        String primaryPermission = "CATALOG";
        String mainAction = (String) context.get("mainAction");
        if (mainAction == null) {
            return ServiceUtil.returnError(UtilProperties.getMessage("CommonUiLabels", "CommonPermissionMainActionAttributeMissing", locale));
        }
        if (security.hasPermission(primaryPermission + "_" + mainAction, userLogin) || security.hasPermission(primaryPermission + "_ADMIN", userLogin)) {
            result.put("hasPermission", Boolean.TRUE);
        } else {
            result.put("hasPermission", Boolean.FALSE);
            result.put("failMessage", UtilProperties.getMessage("CommonUiLabels", "CommonGenericPermissionError", locale));
        }

        return result;
    }


    /**
     * ProdCatalogToParty permission logic
     */
    public static Map<String, Object> prodCatalogToPartyPermissionCheck(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Object primaryPermission = null;
        Map<String, Object> inlineResult = null;
        Object altPermission = "PARTYMGR";
        inlineResult = catalogPermissionCheck(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }

        return result;
    }


    /**
     * create missing category and product alternative urls.
     */
    public static Map<String, Object> createMissingCategoryAndProductAltUrls(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        List<GenericValue> productCategoryRollupList = null;
        Object rootProductCategoryId = null;
        Map<String, Object> inlineResult = null;
        GenericValue productCategoryContent = null;
        Object categoriesUpdated = null;
        Map<String, Object> product = null;
        Object productsNotUpdated = null;
        Object electronicText = null;
        GenericValue productMap = null;
        Map<String, Object> createSimpleTextContentForProductCtx = new HashMap<>();
        Map<String, Object> createSimpleTextContentForCategoryCtx = new HashMap<>();
        List<GenericValue> productCategoryMemberList = null;
        Object resultMap = null;
        Map<String, Object> getContentAndDataResourceCtx = new HashMap<>();
        List<GenericValue> productCategoryContentAndInfoList = null;
        Object categoriesNotUpdated = null;
        List<GenericValue> ProductContentAndInfoList = null;
        Object productsUpdated = null;
        List<GenericValue> productCategoryContentList = null;
        List<Object> parameters_productCategories = null;
        List<GenericValue> productCategoryRollups = null;
        GenericValue productCategoryRollup = null;
        GenericValue productCategory = null;
        Timestamp now = new Timestamp(System.currentTimeMillis());
        result.put("prodCatalogId", context.get("prodCatalogId"));
        categoriesNotUpdated = 0;
        categoriesUpdated = 0;
        productsNotUpdated = 0;
        productsUpdated = 0;
        List<GenericValue> prodCatalogCategoryList = null;
        try {
            prodCatalogCategoryList = EntityQuery.use(delegator)
                    .from("ProdCatalogCategory")
                    .where(UtilMisc.toMap("prodCatalogId", context.get("prodCatalogId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        context.put("productCategories", (List) GroovyUtil.eval("[]", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context)));
        if (prodCatalogCategoryList != null) {
            for (GenericValue prodCatalogCategory : prodCatalogCategoryList) {
                rootProductCategoryId = prodCatalogCategory.get("productCategoryId");
                try {
                    productCategoryRollupList = EntityQuery.use(delegator)
                            .from("ProductCategoryRollup")
                            .where(UtilMisc.toMap("parentProductCategoryId", rootProductCategoryId))
                            .filterByDate()
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                context.put("parentProductCategoryId", rootProductCategoryId);
                context.put("productCategoryRollups", productCategoryRollups);
                context.put("productCategoryRollup", productCategoryRollup);
                context.put("productCategory", productCategory);
                createMissingCategoryAltUrlInline(dctx, context);
                parameters_productCategories = (List<Object>) context.get("parameters_productCategories");
                context = (Map<String, Object>) context.get("context");
                inlineResult = (Map<String, Object>) context.get("inlineResult");
            }
        }
        if (context.get("productCategories") != null) {
            for (GenericValue productCategoryList : (List<GenericValue>) context.get("productCategories")) {
                if (UtilValidate.isEmpty(context.get("category"))) {
                    try {
                        productCategoryContentAndInfoList = EntityQuery.use(delegator)
                                .from("ProductCategoryContentAndInfo")
                                .cache()
                                .filterByDate()
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying ProductCategoryContentAndInfo: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    if (UtilValidate.isEmpty(productCategoryContentAndInfoList)) {
                        createSimpleTextContentForCategoryCtx.put("fromDate", now);
                        createSimpleTextContentForCategoryCtx.put("prodCatContentTypeId", "ALTERNATIVE_URL");
                        createSimpleTextContentForCategoryCtx.put("localeString", "en");
                        createSimpleTextContentForCategoryCtx.put("productCategoryId", productCategoryList.get("productCategoryId"));
                        if (UtilValidate.isEmpty(productCategoryList.get("categoryName"))) {
                            try {
                                productCategoryContentList = EntityQuery.use(delegator)
                                        .from("ProductCategoryContentAndInfo")
                                        .cache()
                                        .filterByDate()
                                        .queryList();
                            } catch (Exception e) {
                                Debug.logError(e, "Error querying ProductCategoryContentAndInfo: " + e.getMessage(), MODULE);
                                return ServiceUtil.returnError(e.getMessage());
                            }
                            if (UtilValidate.isNotEmpty(productCategoryContentList)) {
                                productCategoryContent = EntityUtil.getFirst((List<GenericValue>) productCategoryContentList);
                                getContentAndDataResourceCtx.put("contentId", productCategoryContent.get("contentId"));
                                try {
                                    Map<String, Object> serviceResult = dispatcher.runSync("getContentAndDataResource", getContentAndDataResourceCtx);
                                    if (ServiceUtil.isError(serviceResult)) {
                                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                                    }
                                    resultMap = serviceResult.get("resultData");
                                } catch (Exception e) {
                                    Debug.logError(e, "Error calling getContentAndDataResource: " + e.getMessage(), MODULE);
                                    return ServiceUtil.returnError(e.getMessage());
                                }
                                electronicText = ((Map<String, Object>) resultMap).get("electronicText");
                                createSimpleTextContentForCategoryCtx.put("text", ((Map<String, Object>) electronicText).get("textData"));
                            }
                        } else {
                            createSimpleTextContentForCategoryCtx.put("text", productCategoryList.get("categoryName"));
                        }
                        if (UtilValidate.isNotEmpty(((Map<String, Object>) createSimpleTextContentForCategoryCtx).get("text"))) {
                            try {
                                Map<String, Object> serviceResult = dispatcher.runSync("createSimpleTextContentForCategory", createSimpleTextContentForCategoryCtx);
                                if (ServiceUtil.isError(serviceResult)) {
                                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                                }
                            } catch (Exception e) {
                                Debug.logError(e, "Error calling createSimpleTextContentForCategory: " + e.getMessage(), MODULE);
                                return ServiceUtil.returnError(e.getMessage());
                            }
                            categoriesUpdated = (new BigDecimal(categoriesUpdated.toString())).intValue();
                        }
                        if (!error_list.isEmpty()) {
                            return ServiceUtil.returnError(error_list);
                        }
                    } else {
                        categoriesNotUpdated = (new BigDecimal(categoriesNotUpdated.toString())).intValue();
                    }
                }
                if (UtilValidate.isEmpty(product)) {
                    try {
                        productCategoryMemberList = EntityQuery.use(delegator)
                                .from("ProductCategoryMember")
                                .cache()
                                .filterByDate()
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying ProductCategoryMember: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    if (productCategoryMemberList != null) {
                        for (GenericValue productCategoryMember : productCategoryMemberList) {
                            product.put("productId", productCategoryMember.get("productId"));
                            try {
                                ProductContentAndInfoList = EntityQuery.use(delegator)
                                        .from("ProductContentAndInfo")
                                        .cache()
                                        .filterByDate()
                                        .queryList();
                            } catch (Exception e) {
                                Debug.logError(e, "Error querying ProductContentAndInfo: " + e.getMessage(), MODULE);
                                return ServiceUtil.returnError(e.getMessage());
                            }
                            if (UtilValidate.isEmpty(ProductContentAndInfoList)) {
                                try {
                                    productMap = EntityQuery.use(delegator)
                                            .from("Product")
                                            .where(UtilMisc.toMap("productId", ((Map<String, Object>) product).get("productId")))
                                            .queryOne();
                                } catch (Exception e) {
                                    Debug.logError(e, "Error querying Product: " + e.getMessage(), MODULE);
                                    return ServiceUtil.returnError(e.getMessage());
                                }
                                createSimpleTextContentForProductCtx.put("fromDate", now);
                                createSimpleTextContentForProductCtx.put("productContentTypeId", "ALTERNATIVE_URL");
                                createSimpleTextContentForProductCtx.put("localeString", "en");
                                createSimpleTextContentForProductCtx.put("productId", ((Map<String, Object>) product).get("productId"));
                                if (UtilValidate.isEmpty(productMap.get("internalName"))) {
                                    createSimpleTextContentForProductCtx.put("text", productMap.get("productName"));
                                } else {
                                    createSimpleTextContentForProductCtx.put("text", productMap.get("internalName"));
                                }
                                if (UtilValidate.isNotEmpty(((Map<String, Object>) createSimpleTextContentForProductCtx).get("text"))) {
                                    try {
                                        Map<String, Object> serviceResult = dispatcher.runSync("createSimpleTextContentForProduct", createSimpleTextContentForProductCtx);
                                        if (ServiceUtil.isError(serviceResult)) {
                                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                                        }
                                    } catch (Exception e) {
                                        Debug.logError(e, "Error calling createSimpleTextContentForProduct: " + e.getMessage(), MODULE);
                                        return ServiceUtil.returnError(e.getMessage());
                                    }
                                    productsUpdated = (new BigDecimal(productsUpdated.toString())).intValue();
                                }
                                if (!error_list.isEmpty()) {
                                    return ServiceUtil.returnError(error_list);
                                }
                            } else {
                                productsNotUpdated = (new BigDecimal(productsNotUpdated.toString())).intValue();
                            }
                        }
                    }
                }
            }
        }
        Object categoriesUpdatedMessage = "Categories updated: " + categoriesUpdated;
        List<Object> successMessageList = new LinkedList<>();
        successMessageList.add(categoriesUpdatedMessage);
        Object productsUpdatedMessage = "Products updated: " + productsUpdated;
        successMessageList.add(productsUpdatedMessage);
        result.put("categoriesNotUpdated", categoriesNotUpdated);
        result.put("productsNotUpdated", productsNotUpdated);
        result.put("categoriesUpdated", categoriesUpdated);
        result.put("productsUpdated", productsUpdated);

        return result;
    }


    /**
     * create missing category alternative inline
     */
    public static Map<String, Object> createMissingCategoryAltUrlInline(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        List<Object> parameters_productCategories = null;
        Map<String, Object> inlineResult = null;
        GenericValue productCategory = null;
        List<GenericValue> productCategoryRollups = null;
        GenericValue productCategoryRollup = null;
        try {
            productCategoryRollups = EntityQuery.use(delegator)
                    .from("ProductCategoryRollup")
                    .where(UtilMisc.toMap("parentProductCategoryId", context.get("parentProductCategoryId")))
                    .filterByDate()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (productCategoryRollups != null) {
            for (GenericValue productCategoryRollup_iter : productCategoryRollups) {
                productCategoryRollup = productCategoryRollup_iter;
                try {
                    productCategory = EntityQuery.use(delegator)
                            .from("ProductCategory")
                            .where(UtilMisc.toMap("productCategoryId", productCategoryRollup.get("productCategoryId")))
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying ProductCategory: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                parameters_productCategories.add(productCategory);
                context.put("parentProductCategoryId", productCategoryRollup.get("productCategoryId"));
                context.put("productCategoryRollups", productCategoryRollups);
                context.put("productCategoryRollup", productCategoryRollup);
                context.put("productCategory", productCategory);
                createMissingCategoryAltUrlInline(dctx, context);
                parameters_productCategories = (List<Object>) context.get("parameters_productCategories");
                context = (Map<String, Object>) context.get("context");
                inlineResult = (Map<String, Object>) context.get("inlineResult");
                if (!error_list.isEmpty()) {
                    return ServiceUtil.returnError(error_list);
                }
            }
        }

        return result;
    }


    /**
     */
    public static Map<String, Object> createProdCatalogAndStoreAssocVersatile(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> createCtx = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "createCtx" for service "createProdCatalogAndStoreAssoc"
        createCtx.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createProdCatalogAndStoreAssoc", createCtx);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            context.put("prodCatalogId", serviceResult.get("prodCatalogId"));
            result.put("prodCatalogId", serviceResult.get("prodCatalogId"));
            result.put("productStoreId", serviceResult.get("productStoreId"));
            result.put("fromDate", serviceResult.get("fromDate"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createProdCatalogAndStoreAssoc: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     */
    public static Map<String, Object> updateProdCatalogAndStoreAssocVersatile(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> updateCtx = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "updateCtx" for service "updateProdCatalogAndStoreAssoc"
        updateCtx.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateProdCatalogAndStoreAssoc", updateCtx);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateProdCatalogAndStoreAssoc: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     */
    public static Map<String, Object> deleteProdCatalogStoreAssocVersatile(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> deleteAssocCtx = new HashMap<>();
        Map<String, Object> updateCtx = new HashMap<>();
        if ("expire".equals(context.get("deleteAssocMode"))) {
            // set-service-fields from "parameters" to "updateCtx" for service "updateProductStoreCatalog"
            updateCtx.putAll(UtilMisc.toMap(context));
            Timestamp updateCtx_thruDate = new Timestamp(System.currentTimeMillis());
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updateProductStoreCatalog", updateCtx);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling updateProductStoreCatalog: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        } else {
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
        }

        return result;
    }


    /**
     */
    public static Map<String, Object> deleteProdCatalogAndRelatedVersatile(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        String errMsgStr = null;
        List<GenericValue> values = null;
        GenericValue prodCatalog = null;
        try {
            prodCatalog = EntityQuery.use(delegator)
                    .from("ProdCatalog")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ProdCatalog: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(prodCatalog)) {
            errMsgStr = UtilProperties.getMessage("ProductUiLabels", "ProductCategoryNotFoundForCategoryID", locale);
            error_list.add("${errMsgStr}: ${parameters.prodCatalogId}");
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        }
        Map<String, Object> prodCatIdMap = new HashMap<String, Object>();
        prodCatIdMap.put("prodCatalogId", context.get("prodCatalogId"));
        if ("expired".equals(context.get("deleteParentAssocSelect"))) {
            try {
                values = EntityQuery.use(delegator)
                        .from("ProductStoreCatalog")
                        .where(UtilMisc.toMap("prodCatalogId", ((Map<String, Object>) prodCatIdMap).get("prodCatalogId")))
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
                delegator.removeByAnd("ProductStoreCatalog", prodCatIdMap);
            } catch (Exception e) {
                Debug.logError(e, "Error removing ProductStoreCatalog: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        if ("recursive".equals(context.get("deleteChildrenSelect"))) {
        }
        if ("expired".equals(context.get("deleteChildAssocSelect"))) {
            try {
                values = EntityQuery.use(delegator)
                        .from("ProdCatalogCategory")
                        .where(UtilMisc.toMap("productCategoryId", ((Map<String, Object>) prodCatIdMap).get("prodCatalogId")))
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
                delegator.removeByAnd("ProdCatalogCategory", prodCatIdMap);
            } catch (Exception e) {
                Debug.logError(e, "Error removing ProdCatalogCategory: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        if ("expired".equals(context.get("deleteSpecialAssocSelect"))) {
        }
        if ("all".equals(context.get("deleteSpecialAssocSelect"))) {
        }
        try {
            delegator.removeValue(prodCatalog);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     */
    public static Map<String, Object> deleteProdCatalogAndStoreAssocVersatile(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> deleteRecordCtx = new HashMap<>();
        Map<String, Object> deleteAssocCtx = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "deleteAssocCtx" for service "deleteProdCatalogStoreAssocVersatile"
        deleteAssocCtx.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("deleteProdCatalogStoreAssocVersatile", deleteAssocCtx);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling deleteProdCatalogStoreAssocVersatile: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (!Boolean.FALSE.equals(context.get("deleteRecordAndRelated"))) {
            // set-service-fields from "parameters" to "deleteRecordCtx" for service "deleteProdCatalogAndRelatedVersatile"
            deleteRecordCtx.putAll(UtilMisc.toMap(context));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("deleteProdCatalogAndRelatedVersatile", deleteRecordCtx);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling deleteProdCatalogAndRelatedVersatile: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        return result;
    }


    /**
     */
    public static Map<String, Object> addProdCatalogStoreAssocVersatile(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        List<GenericValue> assocList = null;
        try {
            assocList = EntityQuery.use(delegator)
                    .from("ProductStoreCatalog")
                    .where(UtilMisc.toMap("productStoreId", context.get("productStoreId"), "prodCatalogId", context.get("prodCatalogId")))
                    .filterByDate()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(assocList)) {
            {
                String errorMsg = UtilProperties.getMessage("ProductErrorUiLabels", "productservices.could_not_create_catalog_store_assoc_association_exists", locale);
                error_list.add(errorMsg);
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        }
        Map<String, Object> createAssocCtx = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "createAssocCtx" for service "createProductStoreCatalog"
        createAssocCtx.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createProductStoreCatalog", createAssocCtx);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            result.put("prodCatalogId", serviceResult.get("prodCatalogId"));
            result.put("productStoreId", serviceResult.get("productStoreId"));
            result.put("fromDate", serviceResult.get("fromDate"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createProductStoreCatalog: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }

}

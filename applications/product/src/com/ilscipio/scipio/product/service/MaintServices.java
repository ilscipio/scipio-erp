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
package com.ilscipio.scipio.product.service;

import com.ilscipio.scipio.service.def.*;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class MaintServices {

    /**
     * Copy Product Members from one Category to Another, optionally filtering by the given valid date (otherwise no date filtering done), and optionally recursing (if recurse=Y) down the from category
     */
    @Service(
        name = "copyCategoryProductMembers",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "copyCategoryProductMembers",
        description = "Copy Product Members from one Category to Another, optionally filtering by the given valid date (otherwise no date filtering done), and optionally recursing (if recurse=Y) down the from category",
        auth = "true",
        transactionTimeout = "600",
        attributes = {
            @Attribute(name = "productCategoryId", type = "String", mode = "IN"),
            @Attribute(name = "productCategoryIdTo", type = "String", mode = "IN"),
            @Attribute(name = "validDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "recurse", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface CopyCategoryProductMembers {}

    /**
     * Expire All Product Members in a Category optionally using the thruDate specified as the expire date (now timestamp used by default)
     */
    @Service(
        name = "expireAllCategoryProductMembers",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "expireAllCategoryProductMembers",
        description = "Expire All Product Members in a Category optionally using the thruDate specified as the expire date (now timestamp used by default)",
        auth = "true",
        attributes = {
            @Attribute(name = "productCategoryId", type = "String", mode = "IN"),
            @Attribute(name = "thruDate", type = "Timestamp", mode = "IN", optional = "true")
        }
    )
    public interface ExpireAllCategoryProductMembers {}

    /**
     * Remove All Expired Product Members in a Category, optionally uses the valid date instead of now to determine if the member has expired
     */
    @Service(
        name = "removeExpiredCategoryProductMembers",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "removeExpiredCategoryProductMembers",
        description = "Remove All Expired Product Members in a Category, optionally uses the valid date instead of now to determine if the member has expired",
        auth = "true",
        attributes = {
            @Attribute(name = "productCategoryId", type = "String", mode = "IN"),
            @Attribute(name = "validDate", type = "Timestamp", mode = "IN", optional = "true")
        }
    )
    public interface RemoveExpiredCategoryProductMembers {}

    /**
     * Discontinue Virtuals With Discontinued Variants
     */
    @Service(
        name = "discVirtualsWithDiscVariants",
        engine = "java",
        location = "org.ofbiz.product.product.ProductUtilServices",
        invoke = "discVirtualsWithDiscVariants",
        description = "Discontinue Virtuals With Discontinued Variants",
        auth = "true",
        requireNewTransaction = "true",
        transactionTimeout = "36000"
    )
    public interface DiscVirtualsWithDiscVariants {}

    /**
     * Remove Category Members Of Discontinued Products
     */
    @Service(
        name = "removeCategoryMembersOfDiscProducts",
        engine = "java",
        location = "org.ofbiz.product.product.ProductUtilServices",
        invoke = "removeCategoryMembersOfDiscProducts",
        description = "Remove Category Members Of Discontinued Products",
        auth = "true",
        requireNewTransaction = "true",
        transactionTimeout = "36000"
    )
    public interface RemoveCategoryMembersOfDiscProducts {}

    /**
     * Remove Duplicate, excluding fromDate, Category Members that have no thruDate
     */
    @Service(
        name = "removeDuplicateOpenEndedCategoryMembers",
        engine = "java",
        location = "org.ofbiz.product.product.ProductUtilServices",
        invoke = "removeDuplicateOpenEndedCategoryMembers",
        description = "Remove Duplicate, excluding fromDate, Category Members that have no thruDate",
        auth = "true",
        requireNewTransaction = "true",
        transactionTimeout = "36000"
    )
    public interface RemoveDuplicateOpenEndedCategoryMembers {}

    /**
     * Make Stand Alone From Single Variant Virtuals
     */
    @Service(
        name = "makeStandAloneFromSingleVariantVirtuals",
        engine = "java",
        location = "org.ofbiz.product.product.ProductUtilServices",
        invoke = "makeStandAloneFromSingleVariantVirtuals",
        description = "Make Stand Alone From Single Variant Virtuals",
        auth = "true",
        requireNewTransaction = "true",
        transactionTimeout = "36000"
    )
    public interface MakeStandAloneFromSingleVariantVirtuals {}

    /**
     * A service to be called by the make stand alone service to do the operation for one product.
     */
    @Service(
        name = "mergeVirtualWithSingleVariant",
        engine = "java",
        location = "org.ofbiz.product.product.ProductUtilServices",
        invoke = "mergeVirtualWithSingleVariant",
        description = "A service to be called by the make stand alone service to do the operation for one product.",
        auth = "true",
        requireNewTransaction = "true",
        transactionTimeout = "36000",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "removeOld", type = "Boolean", mode = "IN"),
            @Attribute(name = "test", type = "Boolean", mode = "IN", optional = "true")
        }
    )
    public interface MergeVirtualWithSingleVariant {}

    /**
     * Set All Product Image Names; pattern example: /images/products/${size}/${productId}.jpg; defaults to values in the catalog.properties file (image.url.prefix + / + image.filename.format)
     */
    @Service(
        name = "setAllProductImageNames",
        engine = "java",
        location = "org.ofbiz.product.product.ProductUtilServices",
        invoke = "setAllProductImageNames",
        description = "Set All Product Image Names; pattern example: /images/products/${size}/${productId}.jpg; defaults to values in the catalog.properties file (image.url.prefix + / + image.filename.format)",
        auth = "true",
        requireNewTransaction = "true",
        transactionTimeout = "36000",
        attributes = {
            @Attribute(name = "pattern", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface SetAllProductImageNames {}

    /**
     * Set All Product Image Names
     */
    @Service(
        name = "clearAllVirtualProductImageNames",
        engine = "java",
        location = "org.ofbiz.product.product.ProductUtilServices",
        invoke = "clearAllVirtualProductImageNames",
        description = "Set All Product Image Names",
        auth = "true",
        requireNewTransaction = "true",
        transactionTimeout = "36000"
    )
    public interface ClearAllVirtualProductImageNames {}

    /**
     * Attach Product Features To Category Through Groups
     */
    @Service(
        name = "attachProductFeaturesToCategory",
        engine = "java",
        location = "org.ofbiz.product.product.ProductUtilServices",
        invoke = "attachProductFeaturesToCategory",
        description = "Attach Product Features To Category Through Groups",
        auth = "true",
        requireNewTransaction = "true",
        transactionTimeout = "36000",
        attributes = {
            @Attribute(name = "productCategoryId", type = "String", mode = "IN"),
            @Attribute(name = "doSubCategories", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface AttachProductFeaturesToCategory {}

    /**
     * check For Image Urls exists or not for all categories
     */
    @Service(
        name = "checkImageUrlForAllCategories",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/catalog/CatalogServices.xml",
        invoke = "checkImageUrlForAllCategories",
        description = "check For Image Urls exists or not for all categories",
        transactionTimeout = "36000",
        attributes = {
            @Attribute(name = "topCategory", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "excludeEmpty", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "categoriesMap", type = "java.util.Map", mode = "OUT", optional = "true")
        }
    )
    public interface CheckImageUrlForAllCategories {}

    /**
     * Get all categories of a category 
     */
    @Service(
        name = "getAllCategories",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/catalog/CatalogServices.xml",
        invoke = "getAllCategories",
        description = "Get all categories of a category ",
        attributes = {
            @Attribute(name = "topCategory", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "categories", type = "java.util.List", mode = "OUT", optional = "true")
        }
    )
    public interface GetAllCategories {}

    /**
     * Get all related categories of a category 
     */
    @Service(
        name = "getRelatedCategories",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/catalog/CatalogServices.xml",
        invoke = "getRelatedCategories",
        description = "Get all related categories of a category ",
        attributes = {
            @Attribute(name = "parentProductCategoryId", type = "String", mode = "IN"),
            @Attribute(name = "categories", type = "java.util.List", mode = "INOUT", optional = "true")
        }
    )
    public interface GetRelatedCategories {}

    /**
     *  Returns a complete category trail - can be used for exporting proper category trees.          This is mostly useful when used in combination with breadcrumbs,  for building a facetted index tree,          or to export a category tree for migration to another system.         Will create the tree from root point to categoryId.                   This service is not meant to be run on every request.         Its best use is to generate the trail every so often and store somewhere (a lucene/solr tree, entities, cache or so).          
     */
    @Service(
        name = "getCategoryTrail",
        engine = "java",
        location = "org.ofbiz.product.category.CategoryWorker",
        invoke = "getCategoryTrail",
        description = " Returns a complete category trail - can be used for exporting proper category trees. \n        This is mostly useful when used in combination with breadcrumbs,  for building a facetted index tree, \n        or to export a category tree for migration to another system.\n        Will create the tree from root point to categoryId. \n        \n        This service is not meant to be run on every request.\n        Its best use is to generate the trail every so often and store somewhere (a lucene/solr tree, entities, cache or so). \n        ",
        attributes = {
            @Attribute(name = "productCategoryId", type = "String", mode = "IN"),
            @Attribute(name = "trail", type = "java.util.List", mode = "OUT", optional = "true")
        }
    )
    public interface GetCategoryTrail {}

    /**
     * check For Image Urls exists or not for category
     */
    @Service(
        name = "checkImageUrlForCategoryAndProduct",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/catalog/CatalogServices.xml",
        invoke = "checkImageUrlForCategoryAndProduct",
        description = "check For Image Urls exists or not for category",
        attributes = {
            @Attribute(name = "categoryId", type = "String", mode = "IN"),
            @Attribute(name = "fileNotExists", type = "java.util.List", mode = "OUT", optional = "true"),
            @Attribute(name = "fileExists", type = "java.util.List", mode = "OUT", optional = "true")
        }
    )
    public interface CheckImageUrlForCategoryAndProduct {}

    /**
     * check For Image Urls exists or not For Product
     */
    @Service(
        name = "checkImageUrlForCategory",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/catalog/CatalogServices.xml",
        invoke = "checkImageUrlForCategory",
        description = "check For Image Urls exists or not For Product",
        attributes = {
            @Attribute(name = "categoryId", type = "String", mode = "IN"),
            @Attribute(name = "filesImageMap", type = "java.util.Map", mode = "OUT", optional = "true")
        }
    )
    public interface CheckImageUrlForCategory {}

    /**
     * check For Image Urls exists or not For Product
     */
    @Service(
        name = "checkImageUrlForProduct",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/catalog/CatalogServices.xml",
        invoke = "checkImageUrlForProduct",
        description = "check For Image Urls exists or not For Product",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "filesImageMap", type = "java.util.Map", mode = "OUT", optional = "true")
        }
    )
    public interface CheckImageUrlForProduct {}

    /**
     * check For Image Urls exists or not
     */
    @Service(
        name = "checkImageUrl",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/catalog/CatalogServices.xml",
        invoke = "checkImageUrl",
        description = "check For Image Urls exists or not",
        attributes = {
            @Attribute(name = "imageUrl", type = "String", mode = "IN"),
            @Attribute(name = "isExists", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface CheckImageUrl {}

    /**
     * Purge Expired ProductStorePromoAppl Records, by store or global if productStoreId is null
     */
    @Service(
        name = "purgeOldStoreAutoPromos",
        engine = "java",
        location = "org.ofbiz.product.promo.PromoServices",
        invoke = "purgeOldStoreAutoPromos",
        description = "Purge Expired ProductStorePromoAppl Records, by store or global if productStoreId is null",
        auth = "true",
        attributes = {
            @Attribute(name = "productStoreId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface PurgeOldStoreAutoPromos {}

    /**
     * Update Old Inventory To Detail
     */
    @Service(
        name = "updateOldInventoryToDetailAll",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryServices.xml",
        invoke = "updateOldInventoryToDetailAll",
        description = "Update Old Inventory To Detail",
        auth = "true"
    )
    public interface UpdateOldInventoryToDetailAll {}

    /**
     * Update Old Inventory To Detail
     */
    @Service(
        name = "updateOldInventoryToDetailSingle",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryServices.xml",
        invoke = "updateOldInventoryToDetailSingle",
        description = "Update Old Inventory To Detail",
        auth = "true",
        requireNewTransaction = "true",
        attributes = {
            @Attribute(name = "inventoryItem", type = "org.ofbiz.entity.GenericValue", mode = "IN")
        }
    )
    public interface UpdateOldInventoryToDetailSingle {}

}

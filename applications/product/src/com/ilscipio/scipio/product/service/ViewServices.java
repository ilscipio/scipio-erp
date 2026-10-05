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
public class ViewServices {

    /**
     * Gets a product value object.
     */
    @Service(
        name = "getProduct",
        engine = "java",
        location = "org.ofbiz.product.product.ProductServices",
        invoke = "prodFindProduct",
        description = "Gets a product value object.",
        log = "quiet",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "product", type = "org.ofbiz.entity.GenericValue", mode = "OUT")
        }
    )
    public interface GetProduct {}

    /**
     * Gets a list of variant product value objects.
     */
    @Service(
        name = "getProductVariant",
        engine = "java",
        location = "org.ofbiz.product.product.ProductServices",
        invoke = "prodFindSelectedVariant",
        description = "Gets a list of variant product value objects.",
        log = "quiet",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "selectedFeatures", type = "java.util.Map", mode = "IN"),
            @Attribute(name = "products", type = "java.util.List", mode = "OUT")
        }
    )
    public interface GetProductVariant {}

    /**
     * Gets a Set of product features (distinct)
     */
    @Service(
        name = "getProductFeatureSet",
        engine = "java",
        location = "org.ofbiz.product.product.ProductServices",
        invoke = "prodFindFeatureTypes",
        description = "Gets a Set of product features (distinct)",
        log = "quiet",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "productFeatureApplTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "emptyAction", type = "String", mode = "IN", optional = "true", description = "SCIPIO: If result empty, emptyAction determines handling. \n                Possible values: \"warn\" (current default - stock), \"success\", \"fail\", \"error\"."),
            @Attribute(name = "useCache", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true", description = "SCIPIO: explicit use entity cache control (default: true) (2017-12-20)"),
            @Attribute(name = "featureSet", type = "java.util.Set", mode = "OUT")
        }
    )
    public interface GetProductFeatureSet {}

    /**
     * Gets a tree of product variants based on a virtual product and a list of features.
     */
    @Service(
        name = "getProductVariantTree",
        engine = "java",
        location = "org.ofbiz.product.product.ProductServices",
        invoke = "prodMakeFeatureTree",
        description = "Gets a tree of product variants based on a virtual product and a list of features.",
        log = "quiet",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "featureOrder", type = "java.util.Collection", mode = "IN"),
            @Attribute(name = "productStoreId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "checkInventory", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true"),
            @Attribute(name = "unavailableInTree", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If true, out-of-stock items are included in the returned variantTree (SCIPIO)"),
            @Attribute(name = "variantTree", type = "java.util.Map", mode = "OUT", optional = "true"),
            @Attribute(name = "unavailableVariants", type = "java.util.List", mode = "OUT", optional = "true"),
            @Attribute(name = "variantSample", type = "java.util.Map", mode = "OUT", optional = "true"),
            @Attribute(name = "virtualVariant", type = "java.util.List", mode = "OUT", optional = "true")
        }
    )
    public interface GetProductVariantTree {}

    /**
     * Gets a Collection of products from a 'virtual' parent product.
     */
    @Service(
        name = "getAllProductVariants",
        engine = "java",
        location = "org.ofbiz.product.product.ProductServices",
        invoke = "prodFindAllVariants",
        description = "Gets a Collection of products from a 'virtual' parent product.",
        log = "quiet",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "assocProducts", type = "java.util.Collection", mode = "OUT")
        }
    )
    public interface GetAllProductVariants {}

    /**
     *              Finds associated products by the defined type.  Only one of either productId or productIdTo can be supplied,             not both.  If bidirectional is set to true then the passed in productId will be treated as both a productId             and a productIdTo (defaults to false).  If sortDescending is true then assocProducts will be returned sorted             by sequenceNum descending (defaults to false).         
     */
    @Service(
        name = "getAssociatedProducts",
        engine = "java",
        location = "org.ofbiz.product.product.ProductServices",
        invoke = "prodFindAssociatedByType",
        description = "\n            Finds associated products by the defined type.  Only one of either productId or productIdTo can be supplied,\n            not both.  If bidirectional is set to true then the passed in productId will be treated as both a productId\n            and a productIdTo (defaults to false).  If sortDescending is true then assocProducts will be returned sorted\n            by sequenceNum descending (defaults to false).\n        ",
        log = "quiet",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productIdTo", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "checkViewAllow", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "prodCatalogId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "type", type = "String", mode = "IN"),
            @Attribute(name = "bidirectional", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "sortDescending", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "assocProducts", type = "java.util.Collection", mode = "OUT", optional = "true")
        }
    )
    public interface GetAssociatedProducts {}

    /**
     * Gets a Collection of product features (ProductFeatureAndAppl) for a product.
     */
    @Service(
        name = "getProductFeatures",
        engine = "java",
        location = "org.ofbiz.product.product.ProductServices",
        invoke = "prodGetFeatures",
        description = "Gets a Collection of product features (ProductFeatureAndAppl) for a product.",
        log = "quiet",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "type", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "distinct", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productFeatures", type = "java.util.Collection", mode = "OUT")
        }
    )
    public interface GetProductFeatures {}

    /**
     * Finds a list of SupplierProduct entity values based on a productId.               If other parameters are given, they are used to filter the list down.
     */
    @Service(
        name = "getSuppliersForProduct",
        engine = "java",
        location = "org.ofbiz.product.supplier.SupplierProductServices",
        invoke = "getSuppliersForProduct",
        description = "Finds a list of SupplierProduct entity values based on a productId.\n              If other parameters are given, they are used to filter the list down.",
        log = "quiet",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "currencyUomId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "canDropShip", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "useCache", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true", description = "SCIPIO: useCache flag (default: true) - this should be set to false if called during updated services! (added 2017-12-19)"),
            @Attribute(name = "supplierProducts", type = "java.util.List", mode = "OUT")
        }
    )
    public interface GetSuppliersForProduct {}

    /**
     * Takes a list of product feature (either ProductFeature or ProductFeatureAndAppl) and converts             each one for the supplier specified by partyId, changing the description and idCode
     */
    @Service(
        name = "convertFeaturesForSupplier",
        engine = "java",
        location = "org.ofbiz.product.supplier.SupplierProductServices",
        invoke = "convertFeaturesForSupplier",
        description = "Takes a list of product feature (either ProductFeature or ProductFeatureAndAppl) and converts\n            each one for the supplier specified by partyId, changing the description and idCode",
        attributes = {
            @Attribute(name = "productFeatures", type = "java.util.Collection", mode = "IN"),
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "convertedProductFeatures", type = "java.util.Collection", mode = "OUT")
        }
    )
    public interface ConvertFeaturesForSupplier {}

    /**
     * Gets ProductCategoryMembers for the category_id
     */
    @Service(
        name = "getProductCategoryMembers",
        engine = "java",
        location = "org.ofbiz.product.category.CategoryServices",
        invoke = "getCategoryMembers",
        description = "Gets ProductCategoryMembers for the category_id",
        log = "quiet",
        attributes = {
            @Attribute(name = "categoryId", type = "String", mode = "IN"),
            @Attribute(name = "category", type = "org.ofbiz.entity.GenericValue", mode = "OUT"),
            @Attribute(name = "categoryMembers", type = "java.util.Collection", mode = "OUT")
        }
    )
    public interface GetProductCategoryMembers {}

    /**
     * Set the product options for selected product category, mostly used by getDependentDropdownValues
     */
    @Service(
        name = "getAssociatedProductsList",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "getAssociatedProductsList",
        description = "Set the product options for selected product category, mostly used by getDependentDropdownValues",
        log = "quiet",
        attributes = {
            @Attribute(name = "productCategoryId", type = "String", mode = "IN"),
            @Attribute(name = "products", type = "java.util.List", mode = "OUT")
        }
    )
    public interface GetAssociatedProductsList {}

    /**
     * Gets the previous and next product Ids.
     */
    @Service(
        name = "getPreviousNextProducts",
        engine = "java",
        location = "org.ofbiz.product.category.CategoryServices",
        invoke = "getPreviousNextProducts",
        description = "Gets the previous and next product Ids.",
        log = "quiet",
        attributes = {
            @Attribute(name = "categoryId", type = "String", mode = "IN"),
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "activeOnly", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "introductionDateLimit", type = "java.sql.Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "releaseDateLimit", type = "java.sql.Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "orderByFields", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "category", type = "org.ofbiz.entity.GenericValue", mode = "OUT", optional = "true"),
            @Attribute(name = "previousProductId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "nextProductId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface GetPreviousNextProducts {}

    /**
     * Gets a productCategory and a Collection of associated productCategoryMembers and calculates limiting parameters
     */
    @Service(
        name = "getProductCategoryAndLimitedMembers",
        engine = "java",
        location = "org.ofbiz.product.category.CategoryServices",
        invoke = "getProductCategoryAndLimitedMembers",
        description = "Gets a productCategory and a Collection of associated productCategoryMembers and calculates limiting parameters",
        log = "quiet",
        attributes = {
            @Attribute(name = "productCategoryId", type = "String", mode = "IN"),
            @Attribute(name = "prodCatalogId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "defaultViewSize", type = "Integer", mode = "IN"),
            @Attribute(name = "limitView", type = "Boolean", mode = "IN"),
            @Attribute(name = "checkViewAllow", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "viewIndexString", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "viewSizeString", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "useCacheForMembers", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "activeOnly", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "introductionDateLimit", type = "java.sql.Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "releaseDateLimit", type = "java.sql.Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "orderByFields", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "productStoreId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productCategory", type = "org.ofbiz.entity.GenericValue", mode = "OUT", optional = "true"),
            @Attribute(name = "productCategoryMembers", type = "java.util.Collection", mode = "OUT", optional = "true"),
            @Attribute(name = "viewIndex", type = "Integer", mode = "OUT"),
            @Attribute(name = "viewSize", type = "Integer", mode = "OUT"),
            @Attribute(name = "lowIndex", type = "Integer", mode = "OUT"),
            @Attribute(name = "highIndex", type = "Integer", mode = "OUT"),
            @Attribute(name = "listSize", type = "Integer", mode = "OUT")
        }
    )
    public interface GetProductCategoryAndLimitedMembers {}

}

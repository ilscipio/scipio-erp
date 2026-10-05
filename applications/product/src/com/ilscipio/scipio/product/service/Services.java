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
import com.ilscipio.scipio.service.def.Service.GroupInvoke;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class Services {

    @Service(
        name = "interfaceProduct",
        engine = "interface",
        defaultEntityName = "Product",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true", excludeFields = {"createdDate", "createdByUserLogin", "lastModifiedDate", "lastModifiedByUserLogin"})
        },
        overrideAttributes = {
            @OverrideAttribute(name = "description", allowHtml = "any"),
            @OverrideAttribute(name = "longDescription", allowHtml = "any")
        }
    )
    public interface InterfaceProduct {}

    /**
     * Create a Product
     */
    @Service(
        name = "createProduct",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductServices.xml",
        invoke = "createProduct",
        description = "Create a Product",
        defaultEntityName = "Product",
        auth = "true",
        implemented = {@Implements(service = "interfaceProduct")},
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "productTypeId", optional = "false"),
            @OverrideAttribute(name = "internalName", optional = "false")
        }
    )
    public interface CreateProduct {}

    /**
     * Update a Product
     */
    @Service(
        name = "updateProduct",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductServices.xml",
        invoke = "updateProduct",
        description = "Update a Product",
        defaultEntityName = "Product",
        auth = "true",
        implemented = {@Implements(service = "interfaceProduct")},
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface UpdateProduct {}

    /**
     * Update a Product from Quick Admin
     */
    @Service(
        name = "updateProductQuickAdminName",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductServices.xml",
        invoke = "updateProductQuickAdminName",
        description = "Update a Product from Quick Admin",
        defaultEntityName = "Product",
        auth = "true",
        implemented = {@Implements(service = "interfaceProduct")},
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface UpdateProductQuickAdminName {}

    /**
     * Duplicate a Product using a new productId
     */
    @Service(
        name = "duplicateProduct",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductServices.xml",
        invoke = "duplicateProduct",
        description = "Duplicate a Product using a new productId",
        auth = "true",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "oldProductId", type = "String", mode = "IN"),
            @Attribute(name = "newInternalName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "newProductName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "newDescription", type = "String", mode = "IN", optional = "true", allowHtml = "any"),
            @Attribute(name = "newLongDescription", type = "String", mode = "IN", optional = "true", allowHtml = "any"),
            @Attribute(name = "duplicatePrices", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "duplicateIDs", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "duplicateContent", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "duplicateCategoryMembers", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "duplicateAssocs", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "duplicateAttributes", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "duplicateFeatureAppls", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "duplicateInventoryItems", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "removePrices", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "removeIDs", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "removeContent", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "removeCategoryMembers", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "removeAssocs", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "removeAttributes", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "removeFeatureAppls", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "removeInventoryItems", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface DuplicateProduct {}

    /**
     * Copy Virtual Product's data to the Variant Products
     */
    @Service(
        name = "copyToProductVariants",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductServices.xml",
        invoke = "copyToProductVariants",
        description = "Copy Virtual Product's data to the Variant Products",
        auth = "true",
        attributes = {
            @Attribute(name = "virtualProductId", type = "String", mode = "IN"),
            @Attribute(name = "removeBefore", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "duplicateProduct", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "duplicatePrices", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "duplicateIDs", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "duplicateContent", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "duplicateCategoryMembers", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "duplicateAttributes", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "duplicateFacilities", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "duplicateLocations", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface CopyToProductVariants {}

    /**
     * Create a new product variant
     */
    @Service(
        name = "quickAddVariant",
        engine = "java",
        location = "org.ofbiz.product.product.ProductServices",
        invoke = "quickAddVariant",
        description = "Create a new product variant",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "productFeatureIds", type = "String", mode = "IN"),
            @Attribute(name = "productVariantId", type = "String", mode = "INOUT"),
            @Attribute(name = "sequenceNum", type = "Long", mode = "IN", optional = "true")
        }
    )
    public interface QuickAddVariant {}

    /**
     * Create a new sales agreement with customer for the product
     */
    @Service(
        name = "createSalesAgreement",
        engine = "group",
        location = "createSalesAgreement",
        description = "Create a new sales agreement with customer for the product",
        auth = "true",
        implemented = {@Implements(service = "createAgreement"), @Implements(service = "createAgreementItem"), @Implements(service = "createAgreementProductAppl")}
    )
    public interface CreateSalesAgreement {}

    /**
     *              This will create a virtual product and return its ID, and associate all of the variants with it.             It will not put the selectable features on the virtual or standard features on the variant.         
     */
    @Service(
        name = "quickCreateVirtualWithVariants",
        engine = "java",
        location = "org.ofbiz.product.product.ProductServices",
        invoke = "quickCreateVirtualWithVariants",
        description = "\n            This will create a virtual product and return its ID, and associate all of the variants with it.\n            It will not put the selectable features on the virtual or standard features on the variant.\n        ",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "variantProductIdsBag", type = "String", mode = "IN"),
            @Attribute(name = "productFeatureIdOne", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productFeatureIdTwo", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productFeatureIdThree", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface QuickCreateVirtualWithVariants {}

    /**
     * Create a ProductKeyword
     */
    @Service(
        name = "createProductKeyword",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ProductKeyword",
        defaultEntityName = "ProductKeyword",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "CREATE")
    )
    public interface CreateProductKeyword {}

    /**
     * Update a ProductKeyword
     */
    @Service(
        name = "updateProductKeyword",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ProductKeyword",
        defaultEntityName = "ProductKeyword",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateProductKeyword {}

    /**
     * Delete a ProductKeyword
     */
    @Service(
        name = "deleteProductKeyword",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ProductKeyword",
        defaultEntityName = "ProductKeyword",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "DELETE")
    )
    public interface DeleteProductKeyword {}

    /**
     * Delete all the keywords of a product
     */
    @Service(
        name = "deleteProductKeywords",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductServices.xml",
        invoke = "deleteProductKeywords",
        description = "Delete all the keywords of a product",
        auth = "true",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "DELETE")
    )
    public interface DeleteProductKeywords {}

    /**
     * Index the Keywords for a Product
     */
    @Service(
        name = "indexProductKeywords",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductServices.xml",
        invoke = "indexProductKeywords",
        description = "Index the Keywords for a Product",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "productInstance", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true")
        }
    )
    public interface IndexProductKeywords {}

    /**
     * Induce all the keywords of a product, ignoring the flag in the Product.autoCreateKeywords flag
     */
    @Service(
        name = "forceIndexProductKeywords",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductServices.xml",
        invoke = "forceIndexProductKeywords",
        description = "Induce all the keywords of a product, ignoring the flag in the Product.autoCreateKeywords flag",
        auth = "true",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "CREATE")
    )
    public interface ForceIndexProductKeywords {}

    /**
     * Discontinue Product Sales
     */
    @Service(
        name = "discontinueProductSales",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductServices.xml",
        invoke = "discontinueProductSales",
        description = "Discontinue Product Sales",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN")
        }
    )
    public interface DiscontinueProductSales {}

    /**
     * count Product View
     */
    @Service(
        name = "countProductView",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductServices.xml",
        invoke = "countProductView",
        description = "count Product View",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "weight", type = "Long", mode = "IN", optional = "true")
        }
    )
    public interface CountProductView {}

    /**
     * Create a product review entity
     */
    @Service(
        name = "createProductReview",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductServices.xml",
        invoke = "createProductReview",
        description = "Create a product review entity",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "ProductReview", mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "productReviewId", type = "String", mode = "OUT")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "productStoreId", optional = "false"),
            @OverrideAttribute(name = "productId", optional = "false"),
            @OverrideAttribute(name = "productRating", optional = "false")
        }
    )
    public interface CreateProductReview {}

    /**
     * Updates a product review record
     */
    @Service(
        name = "updateProductReview",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductServices.xml",
        invoke = "updateProductReview",
        description = "Updates a product review record",
        defaultEntityName = "ProductReview",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateProductReview {}

    /**
     * Updates a product review record
     */
    @Service(
        name = "setProductReviewStatus",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductServices.xml",
        invoke = "setProductReviewStatus",
        description = "Updates a product review record",
        auth = "true",
        attributes = {
            @Attribute(name = "productReviewId", type = "String", mode = "INOUT"),
            @Attribute(name = "statusId", type = "String", mode = "IN")
        }
    )
    public interface SetProductReviewStatus {}

    /**
     * Finds productId(s) corresponding to a product reference, productId or a GoodIdentification idValue
     */
    @Service(
        name = "findProductById",
        engine = "java",
        location = "org.ofbiz.product.product.ProductServices",
        invoke = "findProductById",
        description = "Finds productId(s) corresponding to a product reference, productId or a GoodIdentification idValue",
        auth = "true",
        export = "true",
        attributes = {
            @Attribute(name = "idToFind", type = "String", mode = "IN"),
            @Attribute(name = "goodIdentificationTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "searchProductFirst", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "searchAllId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "product", type = "org.ofbiz.entity.GenericValue", mode = "OUT", optional = "true"),
            @Attribute(name = "productsList", type = "List", mode = "OUT", optional = "true")
        }
    )
    public interface FindProductById {}

    @Service(
        name = "createProductAssoc",
        engine = "entity-auto",
        invoke = "create",
        defaultEntityName = "ProductAssoc",
        auth = "true",
        log = "quiet",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "CREATE")
    )
    public interface CreateProductAssoc {}

    @Service(
        name = "updateProductAssoc",
        engine = "entity-auto",
        invoke = "update",
        defaultEntityName = "ProductAssoc",
        auth = "true",
        log = "quiet",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateProductAssoc {}

    @Service(
        name = "deleteProductAssoc",
        engine = "entity-auto",
        invoke = "delete",
        defaultEntityName = "ProductAssoc",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "DELETE")
    )
    public interface DeleteProductAssoc {}

    /**
     *              Create a Product Price.                           If taxAuthGeoId and taxAuthPartyId are (or taxAuthCombinedId is (SCIPIO)) passed in then the price will be considered a price             with tax included (the priceWithoutTax, priceWithTax, taxAmount, and taxPercentage fields will also be populated).                          If the taxInPrice field is 'Y' then the price field will be left with the tax included (price will be equal to priceWithTax),                         otherwise tax will be removed from the passed in price and the price field will be equal to the priceWithoutTax field.                          If taxAuthGeoId or taxAuthPartyId empty, and taxAuthCombinedId is empty (SCIPIO), then the taxInPrice field will be ignored.         
     */
    @Service(
        name = "createProductPrice",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/price/PriceServices.xml",
        invoke = "createProductPrice",
        description = "\n            Create a Product Price. \n            \n            If taxAuthGeoId and taxAuthPartyId are (or taxAuthCombinedId is (SCIPIO)) passed in then the price will be considered a price\n            with tax included (the priceWithoutTax, priceWithTax, taxAmount, and taxPercentage fields will also be populated).\n            \n            If the taxInPrice field is 'Y' then the price field will be left with the tax included (price will be equal to priceWithTax),            \n            otherwise tax will be removed from the passed in price and the price field will be equal to the priceWithoutTax field.\n            \n            If taxAuthGeoId or taxAuthPartyId empty, and taxAuthCombinedId is empty (SCIPIO), then the taxInPrice field will be ignored.\n        ",
        defaultEntityName = "ProductPrice",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true", excludeFields = {"priceWithoutTax", "priceWithTax", "taxAmount", "createdDate", "createdByUserLogin", "lastModifiedDate", "lastModifiedByUserLogin"})
        },
        attributes = {
            @Attribute(name = "taxAuthCombinedId", type = "String", mode = "IN", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", mode = "INOUT", optional = "true"),
            @OverrideAttribute(name = "price", optional = "false")
        }
    )
    public interface CreateProductPrice {}

    /**
     * Update an ProductPrice
     */
    @Service(
        name = "updateProductPrice",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/price/PriceServices.xml",
        invoke = "updateProductPrice",
        description = "Update an ProductPrice",
        defaultEntityName = "ProductPrice",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true", excludeFields = {"priceWithoutTax", "priceWithTax", "taxAmount", "createdDate", "createdByUserLogin", "lastModifiedDate", "lastModifiedByUserLogin"})
        },
        attributes = {
            @Attribute(name = "oldPrice", type = "BigDecimal", mode = "OUT")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "price", optional = "false")
        }
    )
    public interface UpdateProductPrice {}

    /**
     * Delete an ProductPrice
     */
    @Service(
        name = "deleteProductPrice",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/price/PriceServices.xml",
        invoke = "deleteProductPrice",
        description = "Delete an ProductPrice",
        defaultEntityName = "ProductPrice",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "oldPrice", type = "BigDecimal", mode = "OUT")
        }
    )
    public interface DeleteProductPrice {}

    /**
     * Save History of a ProductPrice Change
     */
    @Service(
        name = "saveProductPriceChange",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/price/PriceServices.xml",
        invoke = "saveProductPriceChange",
        description = "Save History of a ProductPrice Change",
        defaultEntityName = "ProductPrice",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "oldPrice", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "productPriceChangeId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface SaveProductPriceChange {}

    /**
     * Create an ProductPaymentMethodType
     */
    @Service(
        name = "createProductPaymentMethodType",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/price/PriceServices.xml",
        invoke = "createProductPaymentMethodType",
        description = "Create an ProductPaymentMethodType",
        defaultEntityName = "ProductPaymentMethodType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "true")
        }
    )
    public interface CreateProductPaymentMethodType {}

    /**
     * Update an ProductPaymentMethodType
     */
    @Service(
        name = "updateProductPaymentMethodType",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/price/PriceServices.xml",
        invoke = "updateProductPaymentMethodType",
        description = "Update an ProductPaymentMethodType",
        defaultEntityName = "ProductPaymentMethodType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateProductPaymentMethodType {}

    /**
     * Delete an ProductPaymentMethodType
     */
    @Service(
        name = "deleteProductPaymentMethodType",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/price/PriceServices.xml",
        invoke = "deleteProductPaymentMethodType",
        description = "Delete an ProductPaymentMethodType",
        defaultEntityName = "ProductPaymentMethodType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteProductPaymentMethodType {}

    /**
     * Create a GoodIdentification
     */
    @Service(
        name = "createGoodIdentification",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a GoodIdentification",
        defaultEntityName = "GoodIdentification",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "CREATE")
    )
    public interface CreateGoodIdentification {}

    /**
     * Update a GoodIdentification
     */
    @Service(
        name = "updateGoodIdentification",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a GoodIdentification",
        defaultEntityName = "GoodIdentification",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateGoodIdentification {}

    /**
     * Delete a GoodIdentification
     */
    @Service(
        name = "deleteGoodIdentification",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a GoodIdentification",
        defaultEntityName = "GoodIdentification",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "DELETE")
    )
    public interface DeleteGoodIdentification {}

    /**
     * Create a ProductGlAccount
     */
    @Service(
        name = "createProductGlAccount",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ProductGlAccount",
        defaultEntityName = "ProductGlAccount",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "CREATE")
    )
    public interface CreateProductGlAccount {}

    /**
     * Update a ProductGlAccount
     */
    @Service(
        name = "updateProductGlAccount",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ProductGlAccount",
        defaultEntityName = "ProductGlAccount",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateProductGlAccount {}

    /**
     * Delete a ProductGlAccount
     */
    @Service(
        name = "deleteProductGlAccount",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ProductGlAccount",
        defaultEntityName = "ProductGlAccount",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "DELETE")
    )
    public interface DeleteProductGlAccount {}

    /**
     * Add Content To Product
     */
    @Service(
        name = "createProductContent",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductContentServices.xml",
        invoke = "createProductContent",
        description = "Add Content To Product",
        defaultEntityName = "ProductContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(entityName = "Content", mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "true")
        }
    )
    public interface CreateProductContent {}

    /**
     * Update Content To Product
     */
    @Service(
        name = "updateProductContent",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductContentServices.xml",
        invoke = "updateProductContent",
        description = "Update Content To Product",
        defaultEntityName = "ProductContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(entityName = "Content", mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateProductContent {}

    /**
     * Remove Content From Product
     */
    @Service(
        name = "removeProductContent",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductContentServices.xml",
        invoke = "removeProductContent",
        description = "Remove Content From Product",
        defaultEntityName = "ProductContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface RemoveProductContent {}

    @Service(
        name = "createEmailContentForProduct",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductContentServices.xml",
        invoke = "createEmailContentForProduct",
        defaultEntityName = "ProductContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(entityName = "Content", mode = "IN", optional = "true")
        },
        attributes = {
            @Attribute(name = "subject", type = "String", mode = "IN"),
            @Attribute(name = "plainBody", type = "String", mode = "IN"),
            @Attribute(name = "htmlBody", type = "String", mode = "IN", optional = "true", allowHtml = "any")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "contentId", mode = "INOUT", optional = "true")
        }
    )
    public interface CreateEmailContentForProduct {}

    @Service(
        name = "updateEmailContentForProduct",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductContentServices.xml",
        invoke = "updateEmailContentForProduct",
        defaultEntityName = "ProductContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(entityName = "Content", mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "subjectDataResourceId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "subject", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "plainBodyDataResourceId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "plainBody", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "htmlBodyDataResourceId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "htmlBody", type = "String", mode = "IN", optional = "true", allowHtml = "any")
        }
    )
    public interface UpdateEmailContentForProduct {}

    @Service(
        name = "createDownloadContentForProduct",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductContentServices.xml",
        invoke = "createDownloadContentForProduct",
        defaultEntityName = "ProductContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(entityName = "Content", mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "imageData", type = "java.nio.ByteBuffer", mode = "IN", optional = "true"),
            @Attribute(name = "_imageData_contentType", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "_imageData_fileName", type = "String", mode = "IN", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "productContentTypeId", optional = "false")
        }
    )
    public interface CreateDownloadContentForProduct {}

    @Service(
        name = "updateDownloadContentForProduct",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductContentServices.xml",
        invoke = "updateDownloadContentForProduct",
        defaultEntityName = "ProductContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(entityName = "Content", mode = "IN", optional = "true")
        },
        attributes = {
            @Attribute(name = "fileDataResourceId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "file", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface UpdateDownloadContentForProduct {}

    @Service(
        name = "createSimpleTextContentForProduct",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductContentServices.xml",
        invoke = "createSimpleTextContentForProduct",
        defaultEntityName = "ProductContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(entityName = "Content", mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "text", type = "String", mode = "IN", allowHtml = "any")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "contentId", optional = "true"),
            @OverrideAttribute(name = "fromDate", optional = "true")
        }
    )
    public interface CreateSimpleTextContentForProduct {}

    @Service(
        name = "updateSimpleTextContentForProduct",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductContentServices.xml",
        invoke = "updateSimpleTextContentForProduct",
        defaultEntityName = "ProductContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(entityName = "Content", mode = "IN", optional = "true")
        },
        attributes = {
            @Attribute(name = "textDataResourceId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "text", type = "String", mode = "IN", optional = "true", allowHtml = "any")
        }
    )
    public interface UpdateSimpleTextContentForProduct {}

    @Service(
        name = "addAdditionalViewForProduct",
        engine = "java",
        location = "org.ofbiz.product.product.ProductServices",
        invoke = "addAdditionalViewForProduct",
        defaultEntityName = "ProductContent",
        auth = "true",
        implemented = {@Implements(service = "uploadFileInterface")},
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "contentId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "imageProfile", type = "String", mode = "IN", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "productContentTypeId", optional = "false"),
            @OverrideAttribute(name = "productId", optional = "false")
        }
    )
    public interface AddAdditionalViewForProduct {}

    /**
     * Upload Additional View Images For Product
     */
    @Service(
        name = "uploadProductAdditionalViewImages",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductContentServices.xml",
        invoke = "uploadProductAdditionalViewImages",
        description = "Upload Additional View Images For Product",
        auth = "true",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "INOUT"),
            @Attribute(name = "additionalImageOne", type = "java.nio.ByteBuffer", mode = "IN", optional = "true"),
            @Attribute(name = "_additionalImageOne_fileName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "_additionalImageOne_contentType", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "additionalImageTwo", type = "java.nio.ByteBuffer", mode = "IN", optional = "true"),
            @Attribute(name = "_additionalImageTwo_fileName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "_additionalImageTwo_contentType", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "additionalImageThree", type = "java.nio.ByteBuffer", mode = "IN", optional = "true"),
            @Attribute(name = "_additionalImageThree_fileName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "_additionalImageThree_contentType", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "additionalImageFour", type = "java.nio.ByteBuffer", mode = "IN", optional = "true"),
            @Attribute(name = "_additionalImageFour_fileName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "_additionalImageFour_contentType", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "additionalImageOne_imageProfile", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "additionalImageTwo_imageProfile", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "additionalImageThree_imageProfile", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "additionalImageFour_imageProfile", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "genericContentPermission", mainAction = "UPDATE")
    )
    public interface UploadProductAdditionalViewImages {}

    /**
     * Update Product SEO
     */
    @Service(
        name = "updateContentSEOForProduct",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductContentServices.xml",
        invoke = "updateContentSEOForProduct",
        description = "Update Product SEO",
        auth = "true",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "title", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "metaKeyword", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "metaDescription", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface UpdateContentSEOForProduct {}

    /**
     * Create a new SupplierProduct record
     */
    @Service(
        name = "createSupplierProduct",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/supplier/SupplierProductServices.xml",
        invoke = "createSupplierProduct",
        description = "Create a new SupplierProduct record",
        defaultEntityName = "SupplierProduct",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "supplierProductId", optional = "false"),
            @OverrideAttribute(name = "lastPrice", optional = "false")
        }
    )
    public interface CreateSupplierProduct {}

    /**
     * Update a SupplierProduct record
     */
    @Service(
        name = "updateSupplierProduct",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/supplier/SupplierProductServices.xml",
        invoke = "updateSupplierProduct",
        description = "Update a SupplierProduct record",
        defaultEntityName = "SupplierProduct",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateSupplierProduct {}

    /**
     * Remove a SupplierProduct record
     */
    @Service(
        name = "removeSupplierProduct",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/supplier/SupplierProductServices.xml",
        invoke = "removeSupplierProduct",
        description = "Remove a SupplierProduct record",
        defaultEntityName = "SupplierProduct",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface RemoveSupplierProduct {}

    /**
     * Create a new SupplierProductFeature record
     */
    @Service(
        name = "createSupplierProductFeature",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/supplier/SupplierProductServices.xml",
        invoke = "createSupplierProductFeature",
        description = "Create a new SupplierProductFeature record",
        defaultEntityName = "SupplierProductFeature",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateSupplierProductFeature {}

    /**
     * Update a SupplierProduct record
     */
    @Service(
        name = "updateSupplierProductFeature",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/supplier/SupplierProductServices.xml",
        invoke = "updateSupplierProductFeature",
        description = "Update a SupplierProduct record",
        defaultEntityName = "SupplierProductFeature",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateSupplierProductFeature {}

    /**
     * Remove a SupplierProduct record
     */
    @Service(
        name = "removeSupplierProductFeature",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/supplier/SupplierProductServices.xml",
        invoke = "removeSupplierProductFeature",
        description = "Remove a SupplierProduct record",
        defaultEntityName = "SupplierProductFeature",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface RemoveSupplierProductFeature {}

    /**
     * Finds a list of SupplierProductFeature entities based on the productFeatureId.             If a partyId is given, only product feature information for that supplier party is returned.
     */
    @Service(
        name = "getSupplierProductFeatures",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/supplier/SupplierProductServices.xml",
        invoke = "getSupplierProductFeatures",
        description = "Finds a list of SupplierProductFeature entities based on the productFeatureId.\n            If a partyId is given, only product feature information for that supplier party is returned.",
        attributes = {
            @Attribute(name = "productFeatureId", type = "String", mode = "IN"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "supplierProductFeatures", type = "java.util.List", mode = "OUT")
        }
    )
    public interface GetSupplierProductFeatures {}

    /**
     * Create a ProductMaint
     */
    @Service(
        name = "createProductMaint",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ProductMaint",
        defaultEntityName = "ProductMaint",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "productMaintSeqId", mode = "OUT")
        }
    )
    public interface CreateProductMaint {}

    /**
     * Update a ProductMaint
     */
    @Service(
        name = "updateProductMaint",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ProductMaint",
        defaultEntityName = "ProductMaint",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateProductMaint {}

    /**
     * Delete a ProductMaint
     */
    @Service(
        name = "deleteProductMaint",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ProductMaint",
        defaultEntityName = "ProductMaint",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "DELETE")
    )
    public interface DeleteProductMaint {}

    /**
     * Create a ProductMeter
     */
    @Service(
        name = "createProductMeter",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ProductMeter",
        defaultEntityName = "ProductMeter",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "CREATE")
    )
    public interface CreateProductMeter {}

    /**
     * Update a ProductMeter
     */
    @Service(
        name = "updateProductMeter",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ProductMeter",
        defaultEntityName = "ProductMeter",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateProductMeter {}

    /**
     * Delete a ProductMeter
     */
    @Service(
        name = "deleteProductMeter",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ProductMeter",
        defaultEntityName = "ProductMeter",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "DELETE")
    )
    public interface DeleteProductMeter {}

    /**
     * Create a ProductGeo
     */
    @Service(
        name = "createProductGeo",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ProductGeo",
        defaultEntityName = "ProductGeo",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "CREATE")
    )
    public interface CreateProductGeo {}

    /**
     * Update a ProductGeo
     */
    @Service(
        name = "updateProductGeo",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ProductGeo",
        defaultEntityName = "ProductGeo",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateProductGeo {}

    /**
     * Delete a ProductGeo
     */
    @Service(
        name = "deleteProductGeo",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ProductGeo",
        defaultEntityName = "ProductGeo",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "DELETE")
    )
    public interface DeleteProductGeo {}

    /**
     * Create a Communication Event Product
     */
    @Service(
        name = "createCommunicationEventProduct",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/communication/CommunicationEventServices.xml",
        invoke = "createCommunicationEventProduct",
        description = "Create a Communication Event Product",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "CommunicationEventProduct", mode = "IN", include = "pk")
        }
    )
    public interface CreateCommunicationEventProduct {}

    /**
     * Remove a Communication Event Product
     */
    @Service(
        name = "removeCommunicationEventProduct",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/communication/CommunicationEventServices.xml",
        invoke = "removeCommunicationEventProduct",
        description = "Remove a Communication Event Product",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "CommunicationEventProduct", mode = "IN", include = "pk")
        }
    )
    public interface RemoveCommunicationEventProduct {}

    /**
     * Create a ProdCatalog
     */
    @Service(
        name = "createProdCatalog",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ProdCatalog",
        defaultEntityName = "ProdCatalog",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "catalogPermissionCheck", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "catalogName", optional = "false")
        }
    )
    public interface CreateProdCatalog {}

    /**
     * Update an ProdCatalog
     */
    @Service(
        name = "updateProdCatalog",
        engine = "entity-auto",
        invoke = "update",
        description = "Update an ProdCatalog",
        defaultEntityName = "ProdCatalog",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "catalogPermissionCheck", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "catalogName", optional = "false")
        }
    )
    public interface UpdateProdCatalog {}

    /**
     * Delete an ProdCatalog
     */
    @Service(
        name = "deleteProdCatalog",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete an ProdCatalog",
        defaultEntityName = "ProdCatalog",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "catalogPermissionCheck", mainAction = "DELETE")
    )
    public interface DeleteProdCatalog {}

    /**
     * Add ProductCategory To ProdCatalog
     */
    @Service(
        name = "addProductCategoryToProdCatalog",
        engine = "entity-auto",
        invoke = "create",
        description = "Add ProductCategory To ProdCatalog",
        defaultEntityName = "ProdCatalogCategory",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "catalogPermissionCheck", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "true")
        }
    )
    public interface AddProductCategoryToProdCatalog {}

    /**
     * Add ProductCategory To ProdCatalog
     */
    @Service(
        name = "updateProductCategoryToProdCatalog",
        engine = "entity-auto",
        invoke = "update",
        description = "Add ProductCategory To ProdCatalog",
        defaultEntityName = "ProdCatalogCategory",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "catalogPermissionCheck", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "prodCatalogCategoryTypeId", optional = "false")
        }
    )
    public interface UpdateProductCategoryToProdCatalog {}

    /**
     * Remove ProductCategory From ProdCatalog
     */
    @Service(
        name = "removeProductCategoryFromProdCatalog",
        engine = "entity-auto",
        invoke = "delete",
        description = "Remove ProductCategory From ProdCatalog",
        defaultEntityName = "ProdCatalogCategory",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "catalogPermissionCheck", mainAction = "DELETE")
    )
    public interface RemoveProductCategoryFromProdCatalog {}

    /**
     * Add ProdCatalog To Party
     */
    @Service(
        name = "addProdCatalogToParty",
        engine = "entity-auto",
        invoke = "create",
        description = "Add ProdCatalog To Party",
        defaultEntityName = "ProdCatalogRole",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "prodCatalogToPartyPermissionCheck", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "true")
        }
    )
    public interface AddProdCatalogToParty {}

    /**
     * Add ProdCatalog To Party
     */
    @Service(
        name = "updateProdCatalogToParty",
        engine = "entity-auto",
        invoke = "update",
        description = "Add ProdCatalog To Party",
        defaultEntityName = "ProdCatalogRole",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "prodCatalogToPartyPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateProdCatalogToParty {}

    /**
     * Remove ProdCatalog From Party
     */
    @Service(
        name = "removeProdCatalogFromParty",
        engine = "entity-auto",
        invoke = "delete",
        description = "Remove ProdCatalog From Party",
        defaultEntityName = "ProdCatalogRole",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "prodCatalogToPartyPermissionCheck", mainAction = "DELETE")
    )
    public interface RemoveProdCatalogFromParty {}

    /**
     * Create an ProductCategory
     */
    @Service(
        name = "createProductCategory",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "createProductCategory",
        description = "Create an ProductCategory",
        defaultEntityName = "ProductCategory",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "productCategoryTypeId", optional = "false")
        }
    )
    public interface CreateProductCategory {}

    /**
     * Update an ProductCategory
     */
    @Service(
        name = "updateProductCategory",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "updateProductCategory",
        description = "Update an ProductCategory",
        defaultEntityName = "ProductCategory",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "productCategoryTypeId", optional = "false")
        }
    )
    public interface UpdateProductCategory {}

    /**
     * SCIPIO: Delete a ProductCategory record (if no associations or members)
     */
    @Service(
        name = "deleteProductCategory",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "deleteProductCategory",
        description = "SCIPIO: Delete a ProductCategory record (if no associations or members)",
        defaultEntityName = "ProductCategory",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteProductCategory {}

    /**
     * Duplicate a Product Category using from oldProductCategoryId to a new productCategoryId
     */
    @Service(
        name = "duplicateProductCategory",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "duplicateProductCategory",
        description = "Duplicate a Product Category using from oldProductCategoryId to a new productCategoryId",
        auth = "true",
        attributes = {
            @Attribute(name = "productCategoryId", type = "String", mode = "IN"),
            @Attribute(name = "oldProductCategoryId", type = "String", mode = "IN"),
            @Attribute(name = "duplicateContent", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "duplicateParentRollup", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "duplicateChildRollup", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "duplicateMembers", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "duplicateCatalogs", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "duplicateFeatures", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "duplicateAttributes", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "duplicateRoles", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface DuplicateProductCategory {}

    /**
     * Add Product To Category
     */
    @Service(
        name = "safeAddProductToCategory",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "addProductToCategory",
        description = "Add Product To Category",
        defaultEntityName = "ProductCategoryMember",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "checkCategoryPermissionWithViewPurchaseAllow", mainAction = "CREATE")
    )
    public interface SafeAddProductToCategory {}

    /**
     * Add Product To Multiple Categories
     */
    @Service(
        name = "addProductToCategories",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "addProductToCategories",
        description = "Add Product To Multiple Categories",
        defaultEntityName = "ProductCategoryMember",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", excludeFields = {"productCategoryId"}),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "categories", type = "Object", mode = "IN")
        },
        permissionService = @PermissionService(service = "checkCategoryPermissionWithViewPurchaseAllow", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", mode = "INOUT", optional = "true")
        }
    )
    public interface AddProductToCategories {}

    /**
     * Add Product To Category
     */
    @Service(
        name = "addProductToCategory",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "addProductToCategory",
        description = "Add Product To Category",
        defaultEntityName = "ProductCategoryMember",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "checkCategoryPermissionWithViewPurchaseAllow", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "true")
        }
    )
    public interface AddProductToCategory {}

    /**
     * Update a ProductCategoryMember
     */
    @Service(
        name = "updateProductToCategory",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "updateProductToCategory",
        description = "Update a ProductCategoryMember",
        defaultEntityName = "ProductCategoryMember",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "checkCategoryPermissionWithViewPurchaseAllow", mainAction = "UPDATE")
    )
    public interface UpdateProductToCategory {}

    /**
     * Remove Product From Category
     */
    @Service(
        name = "removeProductFromCategory",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "removeProductFromCategory",
        description = "Remove Product From Category",
        defaultEntityName = "ProductCategoryMember",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "checkCategoryPermissionWithViewPurchaseAllow", mainAction = "DELETE")
    )
    public interface RemoveProductFromCategory {}

    @Service(
        name = "createProductInCategory",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "createProductInCategory",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "ProductCategory", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "Product", mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(entityName = "Product", mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "productFeatureIdByType", type = "java.util.Map", mode = "IN"),
            @Attribute(name = "productFeatureSelectableByType", type = "java.util.Map", mode = "IN"),
            @Attribute(name = "defaultPrice", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "averageCost", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "currencyUomId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface CreateProductInCategory {}

    /**
     * Add Party To Category
     */
    @Service(
        name = "addPartyToCategory",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "addPartyToCategory",
        description = "Add Party To Category",
        auth = "true",
        attributes = {
            @Attribute(name = "productCategoryId", type = "String", mode = "IN"),
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "roleTypeId", type = "String", mode = "IN"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "thruDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "comments", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface AddPartyToCategory {}

    /**
     * Update Party To Category
     */
    @Service(
        name = "updatePartyToCategory",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "updatePartyToCategory",
        description = "Update Party To Category",
        auth = "true",
        attributes = {
            @Attribute(name = "productCategoryId", type = "String", mode = "IN"),
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "roleTypeId", type = "String", mode = "IN"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN"),
            @Attribute(name = "thruDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "comments", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface UpdatePartyToCategory {}

    /**
     * Remove Party From Category
     */
    @Service(
        name = "removePartyFromCategory",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "removePartyFromCategory",
        description = "Remove Party From Category",
        auth = "true",
        attributes = {
            @Attribute(name = "productCategoryId", type = "String", mode = "IN"),
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "roleTypeId", type = "String", mode = "IN"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN")
        }
    )
    public interface RemovePartyFromCategory {}

    /**
     * Add Party To Product
     */
    @Service(
        name = "addPartyToProduct",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductServices.xml",
        invoke = "addPartyToProduct",
        description = "Add Party To Product",
        auth = "true",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "roleTypeId", type = "String", mode = "IN"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "thruDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "sequenceNum", type = "Long", mode = "IN", optional = "true"),
            @Attribute(name = "comments", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface AddPartyToProduct {}

    /**
     * Update Party To Product
     */
    @Service(
        name = "updatePartyToProduct",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductServices.xml",
        invoke = "updatePartyToProduct",
        description = "Update Party To Product",
        auth = "true",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "roleTypeId", type = "String", mode = "IN"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN"),
            @Attribute(name = "thruDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "sequenceNum", type = "Long", mode = "IN", optional = "true"),
            @Attribute(name = "comments", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface UpdatePartyToProduct {}

    /**
     * Remove Party From Product
     */
    @Service(
        name = "removePartyFromProduct",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductServices.xml",
        invoke = "removePartyFromProduct",
        description = "Remove Party From Product",
        auth = "true",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "roleTypeId", type = "String", mode = "IN"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN")
        }
    )
    public interface RemovePartyFromProduct {}

    /**
     * Safe Add ProductCategory To Category (requires fromDate)
     */
    @Service(
        name = "safeAddProductCategoryToCategory",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "addProductCategoryToCategory",
        description = "Safe Add ProductCategory To Category (requires fromDate)",
        auth = "true",
        attributes = {
            @Attribute(name = "productCategoryId", type = "String", mode = "INOUT"),
            @Attribute(name = "parentProductCategoryId", type = "String", mode = "INOUT"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "INOUT"),
            @Attribute(name = "thruDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "sequenceNum", type = "Long", mode = "IN", optional = "true")
        }
    )
    public interface SafeAddProductCategoryToCategory {}

    /**
     * Add ProductCategory To Category
     */
    @Service(
        name = "addProductCategoryToCategory",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "addProductCategoryToCategory",
        description = "Add ProductCategory To Category",
        auth = "true",
        attributes = {
            @Attribute(name = "productCategoryId", type = "String", mode = "INOUT"),
            @Attribute(name = "parentProductCategoryId", type = "String", mode = "INOUT"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "INOUT", optional = "true"),
            @Attribute(name = "thruDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "sequenceNum", type = "Long", mode = "IN", optional = "true")
        }
    )
    public interface AddProductCategoryToCategory {}

    /**
     * Add ProductCategory To Categories
     */
    @Service(
        name = "addProductCategoryToCategories",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "addProductCategoryToCategories",
        description = "Add ProductCategory To Categories",
        auth = "true",
        attributes = {
            @Attribute(name = "productCategoryId", type = "String", mode = "IN"),
            @Attribute(name = "categories", type = "Object", mode = "IN"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "INOUT", optional = "true"),
            @Attribute(name = "thruDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "sequenceNum", type = "Long", mode = "IN", optional = "true")
        }
    )
    public interface AddProductCategoryToCategories {}

    /**
     * Update ProductCategory To Category
     */
    @Service(
        name = "updateProductCategoryToCategory",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "updateProductCategoryToCategory",
        description = "Update ProductCategory To Category",
        auth = "true",
        attributes = {
            @Attribute(name = "productCategoryId", type = "String", mode = "INOUT"),
            @Attribute(name = "parentProductCategoryId", type = "String", mode = "IN"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN"),
            @Attribute(name = "thruDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "sequenceNum", type = "Long", mode = "IN", optional = "true"),
            @Attribute(name = "originalProductCategoryId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface UpdateProductCategoryToCategory {}

    /**
     * Remove ProductCategory From Category
     */
    @Service(
        name = "removeProductCategoryFromCategory",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "removeProductCategoryFromCategory",
        description = "Remove ProductCategory From Category",
        auth = "true",
        attributes = {
            @Attribute(name = "productCategoryId", type = "String", mode = "INOUT"),
            @Attribute(name = "parentProductCategoryId", type = "String", mode = "IN"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN"),
            @Attribute(name = "originalProductCategoryId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface RemoveProductCategoryFromCategory {}

    @Service(
        name = "createProductCategoryAttribute",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "createProductCategoryAttribute",
        defaultEntityName = "ProductCategoryAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateProductCategoryAttribute {}

    @Service(
        name = "updateProductCategoryAttribute",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "updateProductCategoryAttribute",
        defaultEntityName = "ProductCategoryAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateProductCategoryAttribute {}

    @Service(
        name = "deleteProductCategoryAttribute",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "deleteProductCategoryAttribute",
        defaultEntityName = "ProductCategoryAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteProductCategoryAttribute {}

    /**
     * Create a ProductCategoryLink
     */
    @Service(
        name = "createProductCategoryLink",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "createProductCategoryLink",
        description = "Create a ProductCategoryLink",
        defaultEntityName = "ProductCategoryLink",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "productCategoryGenericPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "true"),
            @OverrideAttribute(name = "linkSeqId", optional = "true")
        }
    )
    public interface CreateProductCategoryLink {}

    /**
     * Update a ProductCategoryLink
     */
    @Service(
        name = "updateProductCategoryLink",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "updateProductCategoryLink",
        description = "Update a ProductCategoryLink",
        defaultEntityName = "ProductCategoryLink",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "productCategoryGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateProductCategoryLink {}

    /**
     * Delete a ProductCategoryLink
     */
    @Service(
        name = "deleteProductCategoryLink",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "deleteProductCategoryLink",
        description = "Delete a ProductCategoryLink",
        defaultEntityName = "ProductCategoryLink",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "productCategoryGenericPermission", mainAction = "DELETE")
    )
    public interface DeleteProductCategoryLink {}

    /**
     * Duplicates a named entity from one productCategoryId to another
     */
    @Service(
        name = "duplicateCategoryEntities",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "duplicateCategoryEntities",
        description = "Duplicates a named entity from one productCategoryId to another",
        auth = "true",
        attributes = {
            @Attribute(name = "productCategoryId", type = "String", mode = "IN"),
            @Attribute(name = "productCategoryIdTo", type = "String", mode = "IN"),
            @Attribute(name = "entityName", type = "String", mode = "IN"),
            @Attribute(name = "validDate", type = "Timestamp", mode = "IN", optional = "true")
        }
    )
    public interface DuplicateCategoryEntities {}

    /**
     * Add Content To Category
     */
    @Service(
        name = "createCategoryContent",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryContentServices.xml",
        invoke = "createCategoryContent",
        description = "Add Content To Category",
        defaultEntityName = "ProductCategoryContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(entityName = "Content", mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "true"),
            @OverrideAttribute(name = "contentId", mode = "INOUT", optional = "true")
        }
    )
    public interface CreateCategoryContent {}

    /**
     * Update Content To Category
     */
    @Service(
        name = "updateCategoryContent",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryContentServices.xml",
        invoke = "updateCategoryContent",
        description = "Update Content To Category",
        defaultEntityName = "ProductCategoryContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(entityName = "Content", mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "contentId", mode = "INOUT", optional = "true")
        }
    )
    public interface UpdateCategoryContent {}

    /**
     * Remove Content From Category
     */
    @Service(
        name = "removeCategoryContent",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryContentServices.xml",
        invoke = "removeCategoryContent",
        description = "Remove Content From Category",
        defaultEntityName = "ProductCategoryContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface RemoveCategoryContent {}

    @Service(
        name = "addAdditionalViewForCategory",
        engine = "java",
        location = "org.ofbiz.product.product.ProductServices",
        invoke = "addAdditionalViewForCategory",
        defaultEntityName = "ProductCategoryContent",
        auth = "true",
        implemented = {@Implements(service = "uploadFileInterface")},
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "contentId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "imageProfile", type = "String", mode = "IN", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "prodCatContentTypeId", optional = "false"),
            @OverrideAttribute(name = "productCategoryId", optional = "false")
        }
    )
    public interface AddAdditionalViewForCategory {}

    /**
     * Upload Additional View Images For ProductCategory
     */
    @Service(
        name = "uploadCategoryAdditionalViewImages",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryContentServices.xml",
        invoke = "uploadCategoryAdditionalViewImages",
        description = "Upload Additional View Images For ProductCategory",
        auth = "true",
        attributes = {
            @Attribute(name = "productCategoryId", type = "String", mode = "INOUT"),
            @Attribute(name = "additionalImageOne", type = "java.nio.ByteBuffer", mode = "IN", optional = "true"),
            @Attribute(name = "_additionalImageOne_fileName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "_additionalImageOne_contentType", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "additionalImageTwo", type = "java.nio.ByteBuffer", mode = "IN", optional = "true"),
            @Attribute(name = "_additionalImageTwo_fileName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "_additionalImageTwo_contentType", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "additionalImageThree", type = "java.nio.ByteBuffer", mode = "IN", optional = "true"),
            @Attribute(name = "_additionalImageThree_fileName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "_additionalImageThree_contentType", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "additionalImageFour", type = "java.nio.ByteBuffer", mode = "IN", optional = "true"),
            @Attribute(name = "_additionalImageFour_fileName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "_additionalImageFour_contentType", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "additionalImageOne_imageProfile", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "additionalImageTwo_imageProfile", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "additionalImageThree_imageProfile", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "additionalImageFour_imageProfile", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "genericContentPermission", mainAction = "UPDATE")
    )
    public interface UploadCategoryAdditionalViewImages {}

    @Service(
        name = "createSimpleTextContentForCategory",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryContentServices.xml",
        invoke = "createSimpleTextContentForCategory",
        defaultEntityName = "ProductCategoryContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(entityName = "Content", mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "text", type = "String", mode = "IN", allowHtml = "any")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "contentId", mode = "INOUT", optional = "true"),
            @OverrideAttribute(name = "fromDate", optional = "true")
        }
    )
    public interface CreateSimpleTextContentForCategory {}

    @Service(
        name = "updateSimpleTextContentForCategory",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryContentServices.xml",
        invoke = "updateSimpleTextContentForCategory",
        defaultEntityName = "ProductCategoryContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(entityName = "Content", mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "textDataResourceId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "text", type = "String", mode = "IN", optional = "true", allowHtml = "any")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "contentId", mode = "INOUT", optional = "true")
        }
    )
    public interface UpdateSimpleTextContentForCategory {}

    /**
     * Update SEO Content For Product Category
     */
    @Service(
        name = "updateContentSEOForCategory",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryContentServices.xml",
        invoke = "updateContentSEOForCategory",
        description = "Update SEO Content For Product Category",
        auth = "true",
        attributes = {
            @Attribute(name = "contentId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "productCategoryId", type = "String", mode = "IN"),
            @Attribute(name = "title", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "metaKeyword", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "metaDescription", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface UpdateContentSEOForCategory {}

    /**
     * Create Related URL Content For Product Category
     */
    @Service(
        name = "createRelatedUrlContentForCategory",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryContentServices.xml",
        invoke = "createRelatedUrlContentForCategory",
        description = "Create Related URL Content For Product Category",
        defaultEntityName = "ProductCategoryContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "title", type = "String", mode = "IN"),
            @Attribute(name = "description", type = "String", mode = "IN"),
            @Attribute(name = "url", type = "String", mode = "IN"),
            @Attribute(name = "localeString", type = "String", mode = "IN", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "contentId", mode = "INOUT", optional = "true"),
            @OverrideAttribute(name = "fromDate", optional = "true")
        }
    )
    public interface CreateRelatedUrlContentForCategory {}

    /**
     * Update Related URL Content For Product Category
     */
    @Service(
        name = "updateRelatedUrlContentForCategory",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryContentServices.xml",
        invoke = "updateRelatedUrlContentForCategory",
        description = "Update Related URL Content For Product Category",
        defaultEntityName = "ProductCategoryContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "contentId", type = "String", mode = "IN"),
            @Attribute(name = "dataResourceId", type = "String", mode = "IN"),
            @Attribute(name = "title", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "description", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "url", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "localeString", type = "String", mode = "IN", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "contentId", mode = "INOUT", optional = "true")
        }
    )
    public interface UpdateRelatedUrlContentForCategory {}

    @Service(
        name = "createDownloadContentForCategory",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryContentServices.xml",
        invoke = "createDownloadContentForCategory",
        defaultEntityName = "ProductCategoryContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(entityName = "Content", mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "imageData", type = "java.nio.ByteBuffer", mode = "IN", optional = "true"),
            @Attribute(name = "_imageData_contentType", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "_imageData_fileName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "dataResourceTypeId", type = "String", mode = "IN", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "contentId", mode = "INOUT", optional = "true"),
            @OverrideAttribute(name = "prodCatContentTypeId", optional = "false")
        }
    )
    public interface CreateDownloadContentForCategory {}

    @Service(
        name = "updateDownloadContentForCategory",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryContentServices.xml",
        invoke = "updateDownloadContentForCategory",
        defaultEntityName = "ProductCategoryContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(entityName = "Content", mode = "IN", optional = "true")
        },
        attributes = {
            @Attribute(name = "imageData", type = "java.nio.ByteBuffer", mode = "IN", optional = "true"),
            @Attribute(name = "_imageData_contentType", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "_imageData_fileName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fileDataResourceId", type = "String", mode = "IN", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "contentId", mode = "INOUT", optional = "true")
        }
    )
    public interface UpdateDownloadContentForCategory {}

    /**
     * Create ProductFeature-DataResource
     */
    @Service(
        name = "createProductFeatureDataResource",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductContentServices.xml",
        invoke = "createProductFeatureDataResource",
        description = "Create ProductFeature-DataResource",
        defaultEntityName = "ProductFeatureDataResource",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface CreateProductFeatureDataResource {}

    /**
     * Remove ProductFeature-DataResource
     */
    @Service(
        name = "removeProductFeatureDataResource",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductContentServices.xml",
        invoke = "removeProductFeatureDataResource",
        description = "Remove ProductFeature-DataResource",
        defaultEntityName = "ProductFeatureDataResource",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface RemoveProductFeatureDataResource {}

    @Service(
        name = "createCustomerDigitalDownloadProduct",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/CustomerDigitalDownloadServices.xml",
        invoke = "createCustomerDigitalDownloadProduct",
        auth = "true",
        attributes = {
            @Attribute(name = "productName", type = "String", mode = "IN"),
            @Attribute(name = "productStoreId", type = "String", mode = "IN"),
            @Attribute(name = "price", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "description", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "currencyUomId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productId", type = "String", mode = "OUT"),
            @Attribute(name = "currencyUomId", type = "String", mode = "OUT"),
            @Attribute(name = "minimumOrderQuantity", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "availableFromDate", type = "Timestamp", mode = "OUT")
        }
    )
    public interface CreateCustomerDigitalDownloadProduct {}

    @Service(
        name = "updateCustomerDigitalDownloadProduct",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/CustomerDigitalDownloadServices.xml",
        invoke = "updateCustomerDigitalDownloadProduct",
        auth = "true",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "currencyUomId", type = "String", mode = "IN"),
            @Attribute(name = "minimumOrderQuantity", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "availableFromDate", type = "Timestamp", mode = "IN"),
            @Attribute(name = "productName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "description", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "price", type = "BigDecimal", mode = "IN", optional = "true")
        }
    )
    public interface UpdateCustomerDigitalDownloadProduct {}

    @Service(
        name = "deleteCustomerDigitalDownloadProduct",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/CustomerDigitalDownloadServices.xml",
        invoke = "deleteCustomerDigitalDownloadProduct",
        auth = "true",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN")
        }
    )
    public interface DeleteCustomerDigitalDownloadProduct {}

    @Service(
        name = "addCustomerDigitalDownloadProductFile",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/CustomerDigitalDownloadServices.xml",
        invoke = "addCustomerDigitalDownloadProductFile",
        auth = "true",
        attributes = {
            @Attribute(name = "contentId", type = "String", mode = "IN"),
            @Attribute(name = "productId", type = "String", mode = "IN")
        }
    )
    public interface AddCustomerDigitalDownloadProductFile {}

    @Service(
        name = "removeCustomerDigitalDownloadProductFile",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/CustomerDigitalDownloadServices.xml",
        invoke = "removeCustomerDigitalDownloadProductFile",
        auth = "true",
        attributes = {
            @Attribute(name = "contentId", type = "String", mode = "IN"),
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "productContentTypeId", type = "String", mode = "IN"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN")
        }
    )
    public interface RemoveCustomerDigitalDownloadProductFile {}

    /**
     * Create a ProductConfig
     */
    @Service(
        name = "createProductConfig",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/config/ConfigServices.xml",
        invoke = "createProductConfig",
        description = "Create a ProductConfig",
        defaultEntityName = "ProductConfig",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", mode = "INOUT", optional = "true")
        }
    )
    public interface CreateProductConfig {}

    /**
     * Update a ProductConfig
     */
    @Service(
        name = "updateProductConfig",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/config/ConfigServices.xml",
        invoke = "updateProductConfig",
        description = "Update a ProductConfig",
        defaultEntityName = "ProductConfig",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateProductConfig {}

    /**
     * Delete a ProductConfig
     */
    @Service(
        name = "deleteProductConfig",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/config/ConfigServices.xml",
        invoke = "deleteProductConfig",
        description = "Delete a ProductConfig",
        defaultEntityName = "ProductConfig",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteProductConfig {}

    /**
     * Create a Config Item
     */
    @Service(
        name = "createProductConfigItem",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/config/ConfigServices.xml",
        invoke = "createProductConfigItem",
        description = "Create a Config Item",
        defaultEntityName = "ProductConfigItem",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "OUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "configItemId", mode = "OUT")
        }
    )
    public interface CreateProductConfigItem {}

    /**
     * Update a Config Item
     */
    @Service(
        name = "updateProductConfigItem",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/config/ConfigServices.xml",
        invoke = "updateProductConfigItem",
        description = "Update a Config Item",
        defaultEntityName = "ProductConfigItem",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateProductConfigItem {}

    /**
     * Delete a Config Item
     */
    @Service(
        name = "deleteProductConfigItem",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/config/ConfigServices.xml",
        invoke = "deleteProductConfigItem",
        description = "Delete a Config Item",
        defaultEntityName = "ProductConfigItem",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteProductConfigItem {}

    /**
     * Create a Config Option
     */
    @Service(
        name = "createProductConfigOption",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/config/ConfigServices.xml",
        invoke = "createProductConfigOption",
        description = "Create a Config Option",
        defaultEntityName = "ProductConfigOption",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "OUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "configItemId", type = "String", mode = "IN")
        }
    )
    public interface CreateProductConfigOption {}

    /**
     * Update a Config Option
     */
    @Service(
        name = "updateProductConfigOption",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/config/ConfigServices.xml",
        invoke = "updateProductConfigOption",
        description = "Update a Config Option",
        defaultEntityName = "ProductConfigOption",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateProductConfigOption {}

    /**
     * Delete a Config Option
     */
    @Service(
        name = "deleteProductConfigOption",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/config/ConfigServices.xml",
        invoke = "deleteProductConfigOption",
        description = "Delete a Config Option",
        defaultEntityName = "ProductConfigOption",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteProductConfigOption {}

    /**
     * Create a ProductConfigProduct
     */
    @Service(
        name = "createProductConfigProduct",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/config/ConfigServices.xml",
        invoke = "createProductConfigProduct",
        description = "Create a ProductConfigProduct",
        defaultEntityName = "ProductConfigProduct",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateProductConfigProduct {}

    /**
     * Update a ProductConfigProduct
     */
    @Service(
        name = "updateProductConfigProduct",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/config/ConfigServices.xml",
        invoke = "updateProductConfigProduct",
        description = "Update a ProductConfigProduct",
        defaultEntityName = "ProductConfigProduct",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateProductConfigProduct {}

    /**
     * Delete ProductConfigProduct
     */
    @Service(
        name = "deleteProductConfigProduct",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/config/ConfigServices.xml",
        invoke = "deleteProductConfigProduct",
        description = "Delete ProductConfigProduct",
        defaultEntityName = "ProductConfigProduct",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteProductConfigProduct {}

    /**
     * Add Content To ProductConfigItem
     */
    @Service(
        name = "createProductConfigItemContent",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/config/ProductConfigItemContentServices.xml",
        invoke = "createProductConfigItemContent",
        description = "Add Content To ProductConfigItem",
        defaultEntityName = "ProdConfItemContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(entityName = "Content", mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "true")
        }
    )
    public interface CreateProductConfigItemContent {}

    /**
     * Update Content To ProductConfigItem
     */
    @Service(
        name = "updateProductConfigItemContent",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/config/ProductConfigItemContentServices.xml",
        invoke = "updateProductConfigItemContent",
        description = "Update Content To ProductConfigItem",
        defaultEntityName = "ProdConfItemContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(entityName = "Content", mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateProductConfigItemContent {}

    /**
     * Remove Content From ProductConfigItem
     */
    @Service(
        name = "removeProductConfigItemContent",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/config/ProductConfigItemContentServices.xml",
        invoke = "removeProductConfigItemContent",
        description = "Remove Content From ProductConfigItem",
        defaultEntityName = "ProdConfItemContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface RemoveProductConfigItemContent {}

    @Service(
        name = "createSimpleTextContentForProductConfigItem",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/config/ProductConfigItemContentServices.xml",
        invoke = "createSimpleTextContentForProductConfigItem",
        defaultEntityName = "ProdConfItemContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(entityName = "Content", mode = "IN", optional = "true")
        },
        attributes = {
            @Attribute(name = "configItemId", type = "String", mode = "IN"),
            @Attribute(name = "contentId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "text", type = "String", mode = "IN", allowHtml = "any")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "contentId", optional = "true")
        }
    )
    public interface CreateSimpleTextContentForProductConfigItem {}

    @Service(
        name = "updateSimpleTextContentForProductConfigItem",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/config/ProductConfigItemContentServices.xml",
        invoke = "updateSimpleTextContentForProductConfigItem",
        defaultEntityName = "ProdConfItemContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(entityName = "Content", mode = "IN", optional = "true")
        },
        attributes = {
            @Attribute(name = "textDataResourceId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "text", type = "String", mode = "IN", optional = "true", allowHtml = "any")
        }
    )
    public interface UpdateSimpleTextContentForProductConfigItem {}

    /**
     *              Returns a map of productFeatureTypeId -> List of ProductFeatures.  If supplied with a productFeatureCategoryId,             the returned result is that of all product features in that category.  Otherwise, if supplied with a productFeatureGroupId,             the result is all features of that feature group.  Otherwise, if there is a productId, the returned result is that of all features             applied to that product.  If the optional productFeatureApplTypeId is specified, only features with application of that type             will be returned (this would only make sense along with a productId parameter.)         
     */
    @Service(
        name = "getProductFeaturesByType",
        engine = "java",
        location = "org.ofbiz.product.feature.ProductFeatureServices",
        invoke = "getProductFeaturesByType",
        description = "\n            Returns a map of productFeatureTypeId -> List of ProductFeatures.  If supplied with a productFeatureCategoryId,\n            the returned result is that of all product features in that category.  Otherwise, if supplied with a productFeatureGroupId,\n            the result is all features of that feature group.  Otherwise, if there is a productId, the returned result is that of all features\n            applied to that product.  If the optional productFeatureApplTypeId is specified, only features with application of that type\n            will be returned (this would only make sense along with a productId parameter.)\n        ",
        attributes = {
            @Attribute(name = "productFeatureCategoryId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productFeatureGroupId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productFeatureApplTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productFeatureTypes", type = "List", mode = "OUT"),
            @Attribute(name = "productFeaturesByType", type = "Map", mode = "OUT")
        }
    )
    public interface GetProductFeaturesByType {}

    /**
     *              Takes a productId and a List of ProductFeatureAndAppl entities and returns a List of productIds which are             existing varaints with those features applied.         
     */
    @Service(
        name = "getAllExistingVariants",
        engine = "java",
        location = "org.ofbiz.product.feature.ProductFeatureServices",
        invoke = "getAllExistingVariants",
        description = "\n            Takes a productId and a List of ProductFeatureAndAppl entities and returns a List of productIds which are\n            existing varaints with those features applied.\n        ",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "productFeatureAppls", type = "List", mode = "IN"),
            @Attribute(name = "variantProductIds", type = "List", mode = "OUT")
        }
    )
    public interface GetAllExistingVariants {}

    /**
     *              For a product, returns a List of all possible product feature combinations, based on all SELECTABLE features which             are applied to the product.  featureCombinations is a List of Maps with the following fields:                 defaultVariantProductId -> default productId for the variant, based on idCodes of the features                 curProductFeatureAndAppls -> List of product features to be applied to this variant                 existingVariantProductIds -> List of productIds of variants which already have these features         
     */
    @Service(
        name = "getVariantCombinations",
        engine = "java",
        location = "org.ofbiz.product.feature.ProductFeatureServices",
        invoke = "getVariantCombinations",
        description = "\n            For a product, returns a List of all possible product feature combinations, based on all SELECTABLE features which\n            are applied to the product.  featureCombinations is a List of Maps with the following fields:\n                defaultVariantProductId -> default productId for the variant, based on idCodes of the features\n                curProductFeatureAndAppls -> List of product features to be applied to this variant\n                existingVariantProductIds -> List of productIds of variants which already have these features\n        ",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "featureCombinations", type = "List", mode = "OUT")
        }
    )
    public interface GetVariantCombinations {}

    /**
     * Takes a productCategoryId and a list of product features and resolves all the product category's              product members from virtual to variant
     */
    @Service(
        name = "getCategoryVariantProducts",
        engine = "java",
        location = "org.ofbiz.product.feature.ProductFeatureServices",
        invoke = "getCategoryVariantProducts",
        description = "Takes a productCategoryId and a list of product features and resolves all the product category's\n             product members from virtual to variant",
        attributes = {
            @Attribute(name = "productCategoryId", type = "String", mode = "IN"),
            @Attribute(name = "productFeatures", type = "java.util.List", mode = "IN"),
            @Attribute(name = "products", type = "java.util.List", mode = "OUT")
        }
    )
    public interface GetCategoryVariantProducts {}

    /**
     *              If property is set (catalog.properties) this will re-activate (null discountinue date) on the product             if inventory is available. Triggered via ECA by shipment receipt services         
     */
    @Service(
        name = "updateProductIfAvailableFromShipment",
        engine = "java",
        location = "org.ofbiz.product.product.ProductServices",
        invoke = "updateProductIfAvailableFromShipment",
        description = "\n            If property is set (catalog.properties) this will re-activate (null discountinue date) on the product\n            if inventory is available. Triggered via ECA by shipment receipt services\n        ",
        attributes = {
            @Attribute(name = "inventoryItemId", type = "String", mode = "IN")
        }
    )
    public interface UpdateProductIfAvailableFromShipment {}

    @Service(
        name = "productGenericPermission",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductServices.xml",
        invoke = "productGenericPermission",
        log = "quiet",
        implemented = {@Implements(service = "permissionInterface")}
    )
    public interface ProductGenericPermission {}

    @Service(
        name = "productCategoryGenericPermission",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "productCategoryGenericPermission",
        implemented = {@Implements(service = "permissionInterface")}
    )
    public interface ProductCategoryGenericPermission {}

    @Service(
        name = "productPriceGenericPermission",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductServices.xml",
        invoke = "productPriceGenericPermission",
        implemented = {@Implements(service = "permissionInterface")}
    )
    public interface ProductPriceGenericPermission {}

    @Service(
        name = "checkCategoryPermissionWithViewPurchaseAllow",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "checkCategoryPermissionWithViewPurchaseAllow",
        implemented = {@Implements(service = "permissionInterface")},
        attributes = {
            @Attribute(name = "productCategoryId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface CheckCategoryPermissionWithViewPurchaseAllow {}

    /**
     * Create a ProductAttribute
     */
    @Service(
        name = "createProductAttribute",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ProductAttribute",
        defaultEntityName = "ProductAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "CREATE")
    )
    public interface CreateProductAttribute {}

    /**
     * Update a ProductAttribute
     */
    @Service(
        name = "updateProductAttribute",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ProductAttribute",
        defaultEntityName = "ProductAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateProductAttribute {}

    /**
     * Delete a ProductAttribute
     */
    @Service(
        name = "deleteProductAttribute",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ProductAttribute",
        defaultEntityName = "ProductAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "DELETE")
    )
    public interface DeleteProductAttribute {}

    @Service(
        name = "createVendorProduct",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductServices.xml",
        invoke = "createVendorProduct",
        defaultEntityName = "VendorProduct",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface CreateVendorProduct {}

    @Service(
        name = "deleteVendorProduct",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductServices.xml",
        invoke = "deleteVendorProduct",
        defaultEntityName = "VendorProduct",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteVendorProduct {}

    /**
     * Create a ProductCategoryGlAccount
     */
    @Service(
        name = "createProductCategoryGlAccount",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductServices.xml",
        invoke = "createProductCategoryGlAccount",
        description = "Create a ProductCategoryGlAccount",
        defaultEntityName = "ProductCategoryGlAccount",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk")
        }
    )
    public interface CreateProductCategoryGlAccount {}

    /**
     * Update a ProductCategoryGlAccount
     */
    @Service(
        name = "updateProductCategoryGlAccount",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductServices.xml",
        invoke = "updateProductCategoryGlAccount",
        description = "Update a ProductCategoryGlAccount",
        defaultEntityName = "ProductCategoryGlAccount",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk")
        }
    )
    public interface UpdateProductCategoryGlAccount {}

    /**
     * Delete a ProductCategoryGlAccount
     */
    @Service(
        name = "deleteProductCategoryGlAccount",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductServices.xml",
        invoke = "deleteProductCategoryGlAccount",
        description = "Delete a ProductCategoryGlAccount",
        defaultEntityName = "ProductCategoryGlAccount",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteProductCategoryGlAccount {}

    /**
     * Catalog Permission Checking Logic
     */
    @Service(
        name = "catalogPermissionCheck",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/catalog/CatalogServices.xml",
        invoke = "catalogPermissionCheck",
        description = "Catalog Permission Checking Logic",
        auth = "true",
        implemented = {@Implements(service = "permissionInterface")}
    )
    public interface CatalogPermissionCheck {}

    /**
     * ProdCatalogToParty Permission Checking Logic
     */
    @Service(
        name = "prodCatalogToPartyPermissionCheck",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/catalog/CatalogServices.xml",
        invoke = "prodCatalogToPartyPermissionCheck",
        description = "ProdCatalogToParty Permission Checking Logic",
        implemented = {@Implements(service = "permissionInterface")}
    )
    public interface ProdCatalogToPartyPermissionCheck {}

    /**
     * Create product and inventory item
     */
    @Service(
        name = "productImportFromSpreadsheet",
        engine = "java",
        location = "org.ofbiz.product.spreadsheetimport.ImportProductServices",
        invoke = "productImportFromSpreadsheet",
        description = "Create product and inventory item",
        auth = "true",
        attributes = {
            @Attribute(name = "dirName", type = "java.lang.String", mode = "IN", optional = "true")
        }
    )
    public interface ProductImportFromSpreadsheet {}

    /**
     * Create a WebAnalyticsConfig
     */
    @Service(
        name = "createWebAnalyticsConfig",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a WebAnalyticsConfig",
        defaultEntityName = "WebAnalyticsConfig",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateWebAnalyticsConfig {}

    /**
     * Update a WebAnalyticsConfig
     */
    @Service(
        name = "updateWebAnalyticsConfig",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a WebAnalyticsConfig",
        defaultEntityName = "WebAnalyticsConfig",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateWebAnalyticsConfig {}

    /**
     * Delete a WebAnalyticsConfig
     */
    @Service(
        name = "deleteWebAnalyticsConfig",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a WebAnalyticsConfig",
        defaultEntityName = "WebAnalyticsConfig",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteWebAnalyticsConfig {}

    /**
     * Create a WebAnalyticsType
     */
    @Service(
        name = "createWebAnalyticsType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a WebAnalyticsType",
        defaultEntityName = "WebAnalyticsType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateWebAnalyticsType {}

    /**
     * Update a WebAnalyticsType
     */
    @Service(
        name = "updateWebAnalyticsType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a WebAnalyticsType",
        defaultEntityName = "WebAnalyticsType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateWebAnalyticsType {}

    /**
     * Delete a WebAnalyticsType
     */
    @Service(
        name = "deleteWebAnalyticsType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a WebAnalyticsType",
        defaultEntityName = "WebAnalyticsType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteWebAnalyticsType {}

    /**
     * Create Product Promo Content
     */
    @Service(
        name = "createProductPromoContent",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductContentServices.xml",
        invoke = "createProductPromoContent",
        description = "Create Product Promo Content",
        defaultEntityName = "ProductPromoContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "true")
        }
    )
    public interface CreateProductPromoContent {}

    /**
     * Update Product Promo Content
     */
    @Service(
        name = "updateProductPromoContent",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductContentServices.xml",
        invoke = "updateProductPromoContent",
        description = "Update Product Promo Content",
        defaultEntityName = "ProductPromoContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateProductPromoContent {}

    /**
     * Cancel by the thru date a Product Promo Content
     */
    @Service(
        name = "removeProductPromoContent",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductContentServices.xml",
        invoke = "removeProductPromoContent",
        description = "Cancel by the thru date a Product Promo Content",
        defaultEntityName = "ProductPromoContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface RemoveProductPromoContent {}

    @Service(
        name = "addImageForProductPromo",
        engine = "java",
        location = "org.ofbiz.product.product.ProductServices",
        invoke = "addImageForProductPromo",
        defaultEntityName = "ProductPromoContent",
        auth = "true",
        implemented = {@Implements(service = "uploadFileInterface")},
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "contentId", type = "String", mode = "IN", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "productPromoContentTypeId", optional = "false"),
            @OverrideAttribute(name = "productPromoId", optional = "false")
        }
    )
    public interface AddImageForProductPromo {}

    @Service(
        name = "addMultipleuploadForProduct",
        engine = "java",
        location = "org.ofbiz.product.imagemanagement.ImageManagementServices",
        invoke = "addMultipleuploadForProduct",
        defaultEntityName = "ProductContent",
        auth = "true",
        implemented = {@Implements(service = "uploadFileInterface")},
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "contentId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "dataResourceId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "imageResize", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentFrameId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "dataResourceFrameId", type = "String", mode = "OUT", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "productContentTypeId", optional = "false"),
            @OverrideAttribute(name = "productId", optional = "false")
        }
    )
    public interface AddMultipleuploadForProduct {}

    /**
     * Multiple upload Images For Product
     */
    @Service(
        name = "multipleUploadProductImages",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/imagemanagement/ImageManagementServices.xml",
        invoke = "UploadProductImages",
        description = "Multiple upload Images For Product",
        auth = "true",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "INOUT"),
            @Attribute(name = "imageResize", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "additionalImageOne", type = "java.nio.ByteBuffer", mode = "IN", optional = "true"),
            @Attribute(name = "_additionalImageOne_fileName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "_additionalImageOne_contentType", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "additionalImageTwo", type = "java.nio.ByteBuffer", mode = "IN", optional = "true"),
            @Attribute(name = "_additionalImageTwo_fileName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "_additionalImageTwo_contentType", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "additionalImageThree", type = "java.nio.ByteBuffer", mode = "IN", optional = "true"),
            @Attribute(name = "_additionalImageThree_fileName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "_additionalImageThree_contentType", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "additionalImageFour", type = "java.nio.ByteBuffer", mode = "IN", optional = "true"),
            @Attribute(name = "_additionalImageFour_fileName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "_additionalImageFour_contentType", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "additionalImageFive", type = "java.nio.ByteBuffer", mode = "IN", optional = "true"),
            @Attribute(name = "_additionalImageFive_fileName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "_additionalImageFive_contentType", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "additionalImageSix", type = "java.nio.ByteBuffer", mode = "IN", optional = "true"),
            @Attribute(name = "_additionalImageSix_fileName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "_additionalImageSix_contentType", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "additionalImageSeven", type = "java.nio.ByteBuffer", mode = "IN", optional = "true"),
            @Attribute(name = "_additionalImageSeven_fileName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "_additionalImageSeven_contentType", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "additionalImageEight", type = "java.nio.ByteBuffer", mode = "IN", optional = "true"),
            @Attribute(name = "_additionalImageEight_fileName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "_additionalImageEight_contentType", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "additionalImageNine", type = "java.nio.ByteBuffer", mode = "IN", optional = "true"),
            @Attribute(name = "_additionalImageNine_fileName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "_additionalImageNine_contentType", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "additionalImageTen", type = "java.nio.ByteBuffer", mode = "IN", optional = "true"),
            @Attribute(name = "_additionalImageTen_fileName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "_additionalImageTen_contentType", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "genericContentPermission", mainAction = "CREATE")
    )
    public interface MultipleUploadProductImages {}

    /**
     * Remove Content From Product and File Image
     */
    @Service(
        name = "removeProductContentAndImageFile",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/imagemanagement/ImageManagementServices.xml",
        invoke = "removeProductContentAndImageFile",
        description = "Remove Content From Product and File Image",
        defaultEntityName = "ProductContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface RemoveProductContentAndImageFile {}

    /**
     * Delete Product Content Relationship Entity
     */
    @Service(
        name = "removeProductContentForImageManagement",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/imagemanagement/ImageManagementServices.xml",
        invoke = "removeProductContentForImageManagement",
        description = "Delete Product Content Relationship Entity",
        auth = "true",
        attributes = {
            @Attribute(name = "contentId", type = "String", mode = "IN"),
            @Attribute(name = "productId", type = "String", mode = "IN")
        }
    )
    public interface RemoveProductContentForImageManagement {}

    /**
     * Delete Image File
     */
    @Service(
        name = "removeImageFileForImageManagement",
        engine = "java",
        location = "org.ofbiz.product.imagemanagement.ImageManagementServices",
        invoke = "removeImageFileForImageManagement",
        description = "Delete Image File",
        auth = "true",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "contentId", type = "String", mode = "IN"),
            @Attribute(name = "objectInfo", type = "String", mode = "IN"),
            @Attribute(name = "dataResourceName", type = "String", mode = "IN")
        }
    )
    public interface RemoveImageFileForImageManagement {}

    /**
     * Create Image Frame For Product.
     */
    @Service(
        name = "addImageFrame",
        engine = "java",
        location = "org.ofbiz.product.imagemanagement.FrameImage",
        invoke = "addImageFrame",
        description = "Create Image Frame For Product.",
        auth = "true",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "imageName", type = "String", mode = "IN"),
            @Attribute(name = "imageWidth", type = "String", mode = "IN"),
            @Attribute(name = "imageHeight", type = "String", mode = "IN"),
            @Attribute(name = "frameContentId", type = "String", mode = "IN"),
            @Attribute(name = "frameDataResourceId", type = "String", mode = "IN")
        }
    )
    public interface AddImageFrame {}

    /**
     * Crop Image
     */
    @Service(
        name = "imageCrop",
        engine = "java",
        location = "org.ofbiz.product.imagemanagement.CropImage",
        invoke = "imageCrop",
        description = "Crop Image",
        auth = "true",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "imageName", type = "String", mode = "IN"),
            @Attribute(name = "imageX", type = "String", mode = "IN"),
            @Attribute(name = "imageY", type = "String", mode = "IN"),
            @Attribute(name = "imageW", type = "String", mode = "IN"),
            @Attribute(name = "imageH", type = "String", mode = "IN")
        }
    )
    public interface ImageCrop {}

    /**
     * Rotate Image
     */
    @Service(
        name = "imageRotate",
        engine = "java",
        location = "org.ofbiz.product.imagemanagement.RotateImage",
        invoke = "imageRotate",
        description = "Rotate Image",
        auth = "true",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "imageName", type = "String", mode = "IN"),
            @Attribute(name = "angle", type = "String", mode = "IN")
        }
    )
    public interface ImageRotate {}

    @Service(
        name = "setImageDetail",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/imagemanagement/ImageManagementServices.xml",
        invoke = "setImageDetail",
        defaultEntityName = "ProductContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "sequenceNum", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "drIsPublic", type = "String", mode = "IN", optional = "true", defaultValue = "N"),
            @Attribute(name = "description", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface SetImageDetail {}

    @Service(
        name = "updateStatusImageManagement",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/imagemanagement/ImageManagementServices.xml",
        invoke = "updateStatusImageManagement",
        auth = "true",
        attributes = {
            @Attribute(name = "contentId", type = "String", mode = "IN"),
            @Attribute(name = "checkStatusId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface UpdateStatusImageManagement {}

    @Service(
        name = "addRejectedReasonImageManagement",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/imagemanagement/ImageManagementServices.xml",
        invoke = "addRejectedReasonImageManagement",
        auth = "true",
        attributes = {
            @Attribute(name = "contentId", type = "String", mode = "IN"),
            @Attribute(name = "description", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface AddRejectedReasonImageManagement {}

    @Service(
        name = "createImageContentApproval",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/imagemanagement/ImageManagementServices.xml",
        invoke = "createImageContentApproval",
        auth = "true",
        attributes = {
            @Attribute(name = "contentId", type = "String", mode = "IN")
        }
    )
    public interface CreateImageContentApproval {}

    @Service(
        name = "removeImageContentApproval",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/imagemanagement/ImageManagementServices.xml",
        invoke = "removeImageContentApproval",
        auth = "true",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN")
        }
    )
    public interface RemoveImageContentApproval {}

    /**
     * Resize Images.
     */
    @Service(
        name = "resizeImages",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/imagemanagement/ImageManagementServices.xml",
        invoke = "resizeImages",
        description = "Resize Images.",
        auth = "true",
        attributes = {
            @Attribute(name = "resizeOption", type = "String", mode = "IN"),
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "size", type = "String", mode = "IN")
        }
    )
    public interface ResizeImages {}

    /**
     * Resize Image Of Product.
     */
    @Service(
        name = "resizeImageOfProduct",
        engine = "java",
        location = "org.ofbiz.product.imagemanagement.ImageManagementServices",
        invoke = "resizeImageOfProduct",
        description = "Resize Image Of Product.",
        auth = "true",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "dataResourceName", type = "String", mode = "IN"),
            @Attribute(name = "resizeWidth", type = "String", mode = "IN")
        }
    )
    public interface ResizeImageOfProduct {}

    /**
     * Create New Image Thumbnail.
     */
    @Service(
        name = "createNewImageThumbnail",
        engine = "java",
        location = "org.ofbiz.product.imagemanagement.ImageManagementServices",
        invoke = "createNewImageThumbnail",
        description = "Create New Image Thumbnail.",
        auth = "true",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "contentId", type = "String", mode = "IN"),
            @Attribute(name = "dataResourceName", type = "String", mode = "IN"),
            @Attribute(name = "drObjectInfo", type = "String", mode = "IN"),
            @Attribute(name = "sizeWidth", type = "String", mode = "IN")
        }
    )
    public interface CreateNewImageThumbnail {}

    /**
     * Remove Image By Size.
     */
    @Service(
        name = "removeImageBySize",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/imagemanagement/ImageManagementServices.xml",
        invoke = "removeImageBySize",
        description = "Remove Image By Size.",
        auth = "true",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "mapKey", type = "String", mode = "IN")
        }
    )
    public interface RemoveImageBySize {}

    /**
     * Resize Image Of Product.
     */
    @Service(
        name = "replaceImageToExistImage",
        engine = "java",
        location = "org.ofbiz.product.imagemanagement.ReplaceImage",
        invoke = "replaceImageToExistImage",
        description = "Resize Image Of Product.",
        auth = "true",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "contentIdExist", type = "String", mode = "IN"),
            @Attribute(name = "contentIdReplace", type = "String", mode = "IN"),
            @Attribute(name = "dataResourceNameExist", type = "String", mode = "IN"),
            @Attribute(name = "dataResourceNameReplace", type = "String", mode = "IN")
        }
    )
    public interface ReplaceImageToExistImage {}

    /**
     * Rename Image.
     */
    @Service(
        name = "renameImage",
        engine = "java",
        location = "org.ofbiz.product.imagemanagement.ImageManagementServices",
        invoke = "renameImage",
        description = "Rename Image.",
        auth = "true",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "contentId", type = "String", mode = "IN"),
            @Attribute(name = "drDataResourceName", type = "String", mode = "IN")
        }
    )
    public interface RenameImage {}

    /**
     * Load data of best selling category by week.
     */
    @Service(
        name = "loadBestSellingCategory",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "loadBestSellingCategory",
        description = "Load data of best selling category by week.",
        auth = "true",
        attributes = {
            @Attribute(name = "productStoreId", type = "String", mode = "IN")
        }
    )
    public interface LoadBestSellingCategory {}

    /**
     * Remove products from best selling category.
     */
    @Service(
        name = "RemoveProductFromBestSellCategory",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "RemoveProductFromBestSellCategory",
        description = "Remove products from best selling category.",
        auth = "true",
        attributes = {
            @Attribute(name = "prodCatalogId", type = "String", mode = "IN")
        }
    )
    public interface RemoveProductFromBestSellCategory {}

    /**
     * Add products to best selling category.
     */
    @Service(
        name = "AddProductToBestSellCategory",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "AddProductToBestSellCategory",
        description = "Add products to best selling category.",
        auth = "true",
        attributes = {
            @Attribute(name = "productStoreId", type = "String", mode = "IN"),
            @Attribute(name = "prodCatalogId", type = "String", mode = "IN"),
            @Attribute(name = "week", type = "Long", mode = "IN"),
            @Attribute(name = "year", type = "Long", mode = "IN")
        }
    )
    public interface AddProductToBestSellCategory {}

    /**
     * Find category child.
     */
    @Service(
        name = "FindCategoryChild",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "FindCategoryChild",
        description = "Find category child.",
        auth = "true",
        attributes = {
            @Attribute(name = "productStoreId", type = "String", mode = "IN"),
            @Attribute(name = "productCategoryId", type = "String", mode = "IN"),
            @Attribute(name = "primaryProductCategoryId", type = "String", mode = "IN"),
            @Attribute(name = "week", type = "Long", mode = "IN"),
            @Attribute(name = "year", type = "Long", mode = "IN")
        }
    )
    public interface FindCategoryChild {}

    /**
     * Find best selling product.
     */
    @Service(
        name = "FindBestSellingProduct",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "FindBestSellingProduct",
        description = "Find best selling product.",
        auth = "true",
        attributes = {
            @Attribute(name = "productStoreId", type = "String", mode = "IN"),
            @Attribute(name = "productCategoryId", type = "String", mode = "IN"),
            @Attribute(name = "primaryProductCategoryId", type = "String", mode = "IN"),
            @Attribute(name = "week", type = "Long", mode = "IN"),
            @Attribute(name = "year", type = "Long", mode = "IN"),
            @Attribute(name = "productCategoryId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface FindBestSellingProduct {}

    /**
     * Create missing Category and Product Alternative URLs
     */
    @Service(
        name = "createMissingCategoryAndProductAltUrls",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/catalog/CatalogServices.xml",
        invoke = "createMissingCategoryAndProductAltUrls",
        description = "Create missing Category and Product Alternative URLs",
        auth = "true",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "prodCatalogId", type = "String", mode = "INOUT"),
            @Attribute(name = "category", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "product", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "categoriesNotUpdated", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "categoriesUpdated", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "productsNotUpdated", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "productsUpdated", type = "Integer", mode = "OUT", optional = "true")
        }
    )
    public interface CreateMissingCategoryAndProductAltUrls {}

    /**
     * Create a Market Interest
     */
    @Service(
        name = "createMarketInterest",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a Market Interest",
        defaultEntityName = "MarketInterest",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "CREATE")
    )
    public interface CreateMarketInterest {}

    /**
     * Update a Market Interest
     */
    @Service(
        name = "updateMarketInterest",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Market Interest",
        defaultEntityName = "MarketInterest",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateMarketInterest {}

    /**
     * Delete a Market Interest
     */
    @Service(
        name = "deleteMarketInterest",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a Market Interest",
        defaultEntityName = "MarketInterest",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "DELETE")
    )
    public interface DeleteMarketInterest {}

    /**
     * Create ProductGroupOrder
     */
    @Service(
        name = "createProductGroupOrder",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductServices.xml",
        invoke = "createProductGroupOrder",
        description = "Create ProductGroupOrder",
        defaultEntityName = "ProductGroupOrder",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "OUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateProductGroupOrder {}

    /**
     * Update ProductGroupOrder
     */
    @Service(
        name = "updateProductGroupOrder",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductServices.xml",
        invoke = "updateProductGroupOrder",
        description = "Update ProductGroupOrder",
        defaultEntityName = "ProductGroupOrder",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateProductGroupOrder {}

    /**
     * Delete ProductGroupOrder
     */
    @Service(
        name = "deleteProductGroupOrder",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductServices.xml",
        invoke = "deleteProductGroupOrder",
        description = "Delete ProductGroupOrder",
        defaultEntityName = "ProductGroupOrder",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteProductGroupOrder {}

    /**
     * Create Job For ProductGroupOrder
     */
    @Service(
        name = "createJobForProductGroupOrder",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductServices.xml",
        invoke = "createJobForProductGroupOrder",
        description = "Create Job For ProductGroupOrder",
        defaultEntityName = "ProductGroupOrder",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateJobForProductGroupOrder {}

    /**
     * Check OrderItem For ProductGroupOrder
     */
    @Service(
        name = "checkOrderItemForProductGroupOrder",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductServices.xml",
        invoke = "checkOrderItemForProductGroupOrder",
        description = "Check OrderItem For ProductGroupOrder",
        auth = "true",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN")
        }
    )
    public interface CheckOrderItemForProductGroupOrder {}

    /**
     * Cancle OrderItemGroupOrder
     */
    @Service(
        name = "cancleOrderItemGroupOrder",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductServices.xml",
        invoke = "cancleOrderItemGroupOrder",
        description = "Cancle OrderItemGroupOrder",
        auth = "true",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "orderItemSeqId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface CancleOrderItemGroupOrder {}

    /**
     * Check ProductGroupOrder Expired
     */
    @Service(
        name = "checkProductGroupOrderExpired",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductServices.xml",
        invoke = "checkProductGroupOrderExpired",
        description = "Check ProductGroupOrder Expired",
        auth = "true",
        attributes = {
            @Attribute(name = "groupOrderId", type = "String", mode = "IN")
        }
    )
    public interface CheckProductGroupOrderExpired {}

    /**
     * Create a GoodIdentificationType
     */
    @Service(
        name = "createGoodIdentificationType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a GoodIdentificationType",
        defaultEntityName = "GoodIdentificationType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        }
    )
    public interface CreateGoodIdentificationType {}

    /**
     * Update a GoodIdentificationType
     */
    @Service(
        name = "updateGoodIdentificationType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a GoodIdentificationType",
        defaultEntityName = "GoodIdentificationType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateGoodIdentificationType {}

    /**
     * Delete a GoodIdentificationType
     */
    @Service(
        name = "deleteGoodIdentificationType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a GoodIdentificationType",
        defaultEntityName = "GoodIdentificationType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteGoodIdentificationType {}

    /**
     * Create ProdCatalogCategoryType Record
     */
    @Service(
        name = "createProdCatalogCategoryType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create ProdCatalogCategoryType Record",
        defaultEntityName = "ProdCatalogCategoryType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateProdCatalogCategoryType {}

    /**
     * Update ProdCatalogCategoryType record
     */
    @Service(
        name = "updateProdCatalogCategoryType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update ProdCatalogCategoryType record",
        defaultEntityName = "ProdCatalogCategoryType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateProdCatalogCategoryType {}

    /**
     * Delete ProdCatalogCategoryType Record
     */
    @Service(
        name = "deleteProdCatalogCategoryType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete ProdCatalogCategoryType Record",
        defaultEntityName = "ProdCatalogCategoryType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteProdCatalogCategoryType {}

    /**
     * Create a ProductAssocType
     */
    @Service(
        name = "createProductAssocType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ProductAssocType",
        defaultEntityName = "ProductAssocType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateProductAssocType {}

    /**
     * Update a ProductAssocType
     */
    @Service(
        name = "updateProductAssocType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ProductAssocType",
        defaultEntityName = "ProductAssocType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateProductAssocType {}

    /**
     * Delete a ProductAssocType
     */
    @Service(
        name = "deleteProductAssocType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ProductAssocType",
        defaultEntityName = "ProductAssocType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteProductAssocType {}

    /**
     * Create a ProductCategoryContentType
     */
    @Service(
        name = "createProductCategoryContentType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ProductCategoryContentType",
        defaultEntityName = "ProductCategoryContentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        }
    )
    public interface CreateProductCategoryContentType {}

    /**
     * Update a ProductCategoryContentType
     */
    @Service(
        name = "updateProductCategoryContentType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ProductCategoryContentType",
        defaultEntityName = "ProductCategoryContentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateProductCategoryContentType {}

    /**
     * Delete a ProductCategoryContentType
     */
    @Service(
        name = "deleteProductCategoryContentType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ProductCategoryContentType",
        defaultEntityName = "ProductCategoryContentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteProductCategoryContentType {}

    /**
     * Create a ProductCategoryType
     */
    @Service(
        name = "createProductCategoryType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ProductCategoryType",
        defaultEntityName = "ProductCategoryType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        }
    )
    public interface CreateProductCategoryType {}

    /**
     * Update a GlFiscalType
     */
    @Service(
        name = "updateProductCategoryType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a GlFiscalType",
        defaultEntityName = "ProductCategoryType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateProductCategoryType {}

    /**
     * Delete a ProductCategoryType
     */
    @Service(
        name = "deleteProductCategoryType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ProductCategoryType",
        defaultEntityName = "ProductCategoryType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteProductCategoryType {}

    /**
     * Create a ProductCategoryTypeAttr
     */
    @Service(
        name = "createProductCategoryTypeAttr",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ProductCategoryTypeAttr",
        defaultEntityName = "ProductCategoryTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk")
        }
    )
    public interface CreateProductCategoryTypeAttr {}

    /**
     * Update a ProductCategoryTypeAttr
     */
    @Service(
        name = "updateProductCategoryTypeAttr",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ProductCategoryTypeAttr",
        defaultEntityName = "ProductCategoryTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateProductCategoryTypeAttr {}

    /**
     * Delete a ProductCategoryTypeAttr
     */
    @Service(
        name = "deleteProductCategoryTypeAttr",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ProductCategoryTypeAttr",
        defaultEntityName = "ProductCategoryTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteProductCategoryTypeAttr {}

    /**
     * Create a ProductContentType
     */
    @Service(
        name = "createProductContentType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ProductContentType",
        defaultEntityName = "ProductContentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        }
    )
    public interface CreateProductContentType {}

    /**
     * Update a ProductContentType
     */
    @Service(
        name = "updateProductContentType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ProductContentType",
        defaultEntityName = "ProductContentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateProductContentType {}

    /**
     * Delete a ProductContentType
     */
    @Service(
        name = "deleteProductContentType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ProductContentType",
        defaultEntityName = "ProductContentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteProductContentType {}

    /**
     * Create a ProductMaintType
     */
    @Service(
        name = "createProductMaintType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ProductMaintType",
        defaultEntityName = "ProductMaintType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        }
    )
    public interface CreateProductMaintType {}

    /**
     * Update a ProductMaintType
     */
    @Service(
        name = "updateProductMaintType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ProductMaintType",
        defaultEntityName = "ProductMaintType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateProductMaintType {}

    /**
     * Delete a ProductMaintType
     */
    @Service(
        name = "deleteProductMaintType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ProductMaintType",
        defaultEntityName = "ProductMaintType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteProductMaintType {}

    /**
     * Create a ProductMeterType
     */
    @Service(
        name = "createProductMeterType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ProductMeterType",
        defaultEntityName = "ProductMeterType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        }
    )
    public interface CreateProductMeterType {}

    /**
     * Update a ProductMeterType
     */
    @Service(
        name = "updateProductMeterType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ProductMeterType",
        defaultEntityName = "ProductMeterType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateProductMeterType {}

    /**
     * Delete a ProductMeterType
     */
    @Service(
        name = "deleteProductMeterType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ProductMeterType",
        defaultEntityName = "ProductMeterType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteProductMeterType {}

    /**
     * Create a ProductType
     */
    @Service(
        name = "createProductType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ProductType",
        defaultEntityName = "ProductType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateProductType {}

    /**
     * Update a ProductType
     */
    @Service(
        name = "updateProductType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ProductType",
        defaultEntityName = "ProductType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateProductType {}

    /**
     * Delete a ProductType
     */
    @Service(
        name = "deleteProductType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ProductType",
        defaultEntityName = "ProductType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteProductType {}

    /**
     * Create a ProductTypeAttr
     */
    @Service(
        name = "createProductTypeAttr",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ProductTypeAttr",
        defaultEntityName = "ProductTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateProductTypeAttr {}

    /**
     * Update a ProductTypeAttr
     */
    @Service(
        name = "updateProductTypeAttr",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ProductTypeAttr",
        defaultEntityName = "ProductTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateProductTypeAttr {}

    /**
     * Delete a ProductTypeAttr
     */
    @Service(
        name = "deleteProductTypeAttr",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ProductTypeAttr",
        defaultEntityName = "ProductTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteProductTypeAttr {}

    /**
     * Create a ProdCatalogInvFacility
     */
    @Service(
        name = "createProdCatalogInvFacility",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ProdCatalogInvFacility",
        defaultEntityName = "ProdCatalogInvFacility",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateProdCatalogInvFacility {}

    /**
     * Update a ProdCatalogInvFacility
     */
    @Service(
        name = "updateProdCatalogInvFacility",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ProdCatalogInvFacility",
        defaultEntityName = "ProdCatalogInvFacility",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateProdCatalogInvFacility {}

    /**
     * Expire a ProdCatalogInvFacility Record
     */
    @Service(
        name = "expireProdCatalogInvFacility",
        engine = "entity-auto",
        invoke = "expire",
        description = "Expire a ProdCatalogInvFacility Record",
        defaultEntityName = "ProdCatalogInvFacility",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface ExpireProdCatalogInvFacility {}

    /**
     * SCIPIO: Create a ProdCatalog and ProductStoreCatalog association
     */
    @Service(
        name = "createProdCatalogAndStoreAssoc",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "createProdCatalogAndStoreAssoc",
        description = "SCIPIO: Create a ProdCatalog and ProductStoreCatalog association",
        auth = "true",
        implemented = {@Implements(service = "createProdCatalog"), @Implements(service = "createProductStoreCatalog")}
    )
    public interface CreateProdCatalogAndStoreAssoc {}

    /**
     * SCIPIO: Update a ProdCatalog and ProductStoreCatalog association
     */
    @Service(
        name = "updateProdCatalogAndStoreAssoc",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "updateProdCatalogAndStoreAssoc",
        description = "SCIPIO: Update a ProdCatalog and ProductStoreCatalog association",
        auth = "true",
        implemented = {@Implements(service = "updateProdCatalog"), @Implements(service = "updateProductStoreCatalog")}
    )
    public interface UpdateProdCatalogAndStoreAssoc {}

    /**
     * SCIPIO: Delete a ProdCatalog and ProductStoreCatalog association
     */
    @Service(
        name = "deleteProdCatalogAndStoreAssoc",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "deleteProdCatalogAndStoreAssoc",
        description = "SCIPIO: Delete a ProdCatalog and ProductStoreCatalog association",
        auth = "true",
        implemented = {@Implements(service = "deleteProdCatalog"), @Implements(service = "deleteProductStoreCatalog")}
    )
    public interface DeleteProdCatalogAndStoreAssoc {}

    /**
     * SCIPIO: Create a ProductCategory and its top association to catalog
     */
    @Service(
        name = "createProductCategoryAndCatalogAssoc",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "createProductCategoryAndCatalogAssoc",
        description = "SCIPIO: Create a ProductCategory and its top association to catalog",
        auth = "true",
        implemented = {@Implements(service = "createProductCategory"), @Implements(service = "addProductCategoryToProdCatalog")}
    )
    public interface CreateProductCategoryAndCatalogAssoc {}

    /**
     * SCIPIO: Update a ProductCategory and its top association to catalog
     */
    @Service(
        name = "updateProductCategoryAndCatalogAssoc",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "updateProductCategoryAndCatalogAssoc",
        description = "SCIPIO: Update a ProductCategory and its top association to catalog",
        auth = "true",
        implemented = {@Implements(service = "updateProductCategory"), @Implements(service = "updateProductCategoryToProdCatalog")}
    )
    public interface UpdateProductCategoryAndCatalogAssoc {}

    /**
     * SCIPIO: Create a ProductCategory and add as child of another category
     */
    @Service(
        name = "createProductCategoryAndCategoryAssoc",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "createProductCategoryAndCategoryAssoc",
        description = "SCIPIO: Create a ProductCategory and add as child of another category",
        auth = "true",
        implemented = {@Implements(service = "createProductCategory"), @Implements(service = "addProductCategoryToCategory")}
    )
    public interface CreateProductCategoryAndCategoryAssoc {}

    /**
     * SCIPIO: Update a ProductCategory and its association to another category (as child)
     */
    @Service(
        name = "updateProductCategoryAndCategoryAssoc",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "updateProductCategoryAndCategoryAssoc",
        description = "SCIPIO: Update a ProductCategory and its association to another category (as child)",
        auth = "true",
        implemented = {@Implements(service = "updateProductCategory"), @Implements(service = "updateProductCategoryToCategory")},
        overrideAttributes = {
            @OverrideAttribute(name = "productCategoryId", mode = "IN")
        }
    )
    public interface UpdateProductCategoryAndCategoryAssoc {}

    /**
     * SCIPIO: For each simple-text-compatible prodCatContentTypeId, returns a list of complex record views,              where the first entry is ProductCategoryContentAndElectronicText and the following entries (if any) are ContentAssocToElectronicText views.
     */
    @Service(
        name = "getProductCategoryContentLocalizedSimpleTextViews",
        engine = "java",
        location = "org.ofbiz.product.category.CategoryServices",
        invoke = "getProductCategoryContentLocalizedSimpleTextViews",
        description = "SCIPIO: For each simple-text-compatible prodCatContentTypeId, returns a list of complex record views, \n            where the first entry is ProductCategoryContentAndElectronicText and the following entries (if any) are ContentAssocToElectronicText views.",
        auth = "true",
        attributes = {
            @Attribute(name = "productCategoryId", type = "String", mode = "IN"),
            @Attribute(name = "prodCatContentTypeIdList", type = "List", mode = "IN"),
            @Attribute(name = "filterByDate", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true"),
            @Attribute(name = "useCache", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false"),
            @Attribute(name = "getViewsByType", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true"),
            @Attribute(name = "getViewsByTypeAndLocale", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false"),
            @Attribute(name = "getTextByTypeAndLocale", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false"),
            @Attribute(name = "viewsByType", type = "Map", mode = "OUT", optional = "true"),
            @Attribute(name = "viewsByTypeAndLocale", type = "Map", mode = "OUT", optional = "true"),
            @Attribute(name = "textByTypeAndLocale", type = "Map", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "VIEW")
    )
    public interface GetProductCategoryContentLocalizedSimpleTextViews {}

    /**
     * SCIPIO: For each simple-text-compatible productContentTypeIdList, returns a list of complex record views,              where the first entry is ProductContentAndElectronicText and the following entries (if any) are ContentAssocToElectronicText views.
     */
    @Service(
        name = "getProductContentLocalizedSimpleTextViews",
        engine = "java",
        location = "org.ofbiz.product.product.ProductServices",
        invoke = "getProductContentLocalizedSimpleTextViews",
        description = "SCIPIO: For each simple-text-compatible productContentTypeIdList, returns a list of complex record views, \n            where the first entry is ProductContentAndElectronicText and the following entries (if any) are ContentAssocToElectronicText views.",
        auth = "true",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "productContentTypeIdList", type = "List", mode = "IN"),
            @Attribute(name = "filterByDate", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true"),
            @Attribute(name = "useCache", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false"),
            @Attribute(name = "getViewsByType", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true"),
            @Attribute(name = "getViewsByTypeAndLocale", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false"),
            @Attribute(name = "getTextByTypeAndLocale", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false"),
            @Attribute(name = "viewsByType", type = "Map", mode = "OUT", optional = "true"),
            @Attribute(name = "viewsByTypeAndLocale", type = "Map", mode = "OUT", optional = "true"),
            @Attribute(name = "textByTypeAndLocale", type = "Map", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "VIEW")
    )
    public interface GetProductContentLocalizedSimpleTextViews {}

    /**
     * SCIPIO: Intelligently creates, updates and deletes ProductCategoryContent ALTERNATE_LOCALE simple text contents (wrapper around replaceContentLocalizedSimpleTexts)
     */
    @Service(
        name = "replaceProductCategoryContentLocalizedSimpleTexts",
        engine = "java",
        location = "org.ofbiz.product.category.CategoryServices",
        invoke = "replaceProductCategoryContentLocalizedSimpleTexts",
        description = "SCIPIO: Intelligently creates, updates and deletes ProductCategoryContent ALTERNATE_LOCALE simple text contents (wrapper around replaceContentLocalizedSimpleTexts)",
        implemented = {@Implements(service = "replaceEntityContentLocalizedSimpleTextsInterface")},
        attributes = {
            @Attribute(name = "productCategoryId", type = "String", mode = "IN")
        }
    )
    public interface ReplaceProductCategoryContentLocalizedSimpleTexts {}

    /**
     * SCIPIO: Intelligently creates, updates and deletes ProductContent ALTERNATE_LOCALE simple text contents (wrapper around replaceContentLocalizedSimpleTexts)
     */
    @Service(
        name = "replaceProductContentLocalizedSimpleTexts",
        engine = "java",
        location = "org.ofbiz.product.product.ProductServices",
        invoke = "replaceProductContentLocalizedSimpleTexts",
        description = "SCIPIO: Intelligently creates, updates and deletes ProductContent ALTERNATE_LOCALE simple text contents (wrapper around replaceContentLocalizedSimpleTexts)",
        implemented = {@Implements(service = "replaceEntityContentLocalizedSimpleTextsInterface")},
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN")
        }
    )
    public interface ReplaceProductContentLocalizedSimpleTexts {}

    /**
     * SCIPIO: Create a ProdCatalog and ProductStoreCatalog association (versatile)
     */
    @Service(
        name = "createProdCatalogAndStoreAssocVersatile",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/catalog/CatalogServices.xml",
        invoke = "createProdCatalogAndStoreAssocVersatile",
        description = "SCIPIO: Create a ProdCatalog and ProductStoreCatalog association (versatile)",
        auth = "true",
        implemented = {@Implements(service = "createProdCatalogAndStoreAssoc")}
    )
    public interface CreateProdCatalogAndStoreAssocVersatile {}

    /**
     * SCIPIO: Update a ProdCatalog and ProductStoreCatalog association (versatile)
     */
    @Service(
        name = "updateProdCatalogAndStoreAssocVersatile",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/catalog/CatalogServices.xml",
        invoke = "updateProdCatalogAndStoreAssocVersatile",
        description = "SCIPIO: Update a ProdCatalog and ProductStoreCatalog association (versatile)",
        auth = "true",
        implemented = {@Implements(service = "updateProdCatalogAndStoreAssoc")}
    )
    public interface UpdateProdCatalogAndStoreAssocVersatile {}

    /**
     * SCIPIO: Delete a ProductStoreCatalog association(s) (versatile)
     */
    @Service(
        name = "deleteProdCatalogStoreAssocVersatile",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/catalog/CatalogServices.xml",
        invoke = "deleteProdCatalogStoreAssocVersatile",
        description = "SCIPIO: Delete a ProductStoreCatalog association(s) (versatile)",
        auth = "true",
        implemented = {@Implements(service = "deleteProductStoreCatalog")},
        attributes = {
            @Attribute(name = "deleteAssocMode", type = "String", mode = "IN", optional = "true", defaultValue = "remove", description = "(remove|expire, default: remove)")
        }
    )
    public interface DeleteProdCatalogStoreAssocVersatile {}

    /**
     * SCIPIO: Delete a ProdCatalog record and various related records [WARN: may be dangerous - best-effort] (versatile)
     */
    @Service(
        name = "deleteProdCatalogAndRelatedVersatile",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/catalog/CatalogServices.xml",
        invoke = "deleteProdCatalogAndRelatedVersatile",
        description = "SCIPIO: Delete a ProdCatalog record and various related records [WARN: may be dangerous - best-effort] (versatile)",
        defaultEntityName = "ProdCatalog",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "deleteParentAssocSelect", type = "String", mode = "IN", optional = "true", defaultValue = "all", description = "(none|expired|all, default: expired) Controls whether deletes all (default)\n                or only expired ProductStoreCatalog records to this category's parents"),
            @Attribute(name = "deleteChildAssocSelect", type = "String", mode = "IN", optional = "true", defaultValue = "expired", description = "(none|expired|all, default: expired) If \"expired\" (default), removes children assoc records\n                such as ProdCatalogCategory that are expired; \n                if \"all\", removes all such records even if destructive.\n                WARN: \"all\" mode may leave products/categories as orphans!"),
            @Attribute(name = "deleteSpecialAssocSelect", type = "String", mode = "IN", optional = "true", defaultValue = "none", description = "(none|expired|all, default: expired) Special-purpose association deletion mode\n                WARN: may currently be destructive (TODO: REVIEW) - \"expired\" is safe but only if no expired records are still needed.")
        },
        permissionService = @PermissionService(service = "catalogPermissionCheck", mainAction = "DELETE")
    )
    public interface DeleteProdCatalogAndRelatedVersatile {}

    /**
     * SCIPIO: Delete a ProdCatalog AND/OR ProductStoreCatalog association(s) (versatile)
     */
    @Service(
        name = "deleteProdCatalogAndStoreAssocVersatile",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/catalog/CatalogServices.xml",
        invoke = "deleteProdCatalogAndStoreAssocVersatile",
        description = "SCIPIO: Delete a ProdCatalog AND/OR ProductStoreCatalog association(s) (versatile)",
        auth = "true",
        implemented = {@Implements(service = "deleteProdCatalogStoreAssocVersatile")},
        attributes = {
            @Attribute(name = "deleteRecordAndRelated", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true", description = "If true, attempts to delete ProdCatalog; if false only deletes store association")
        }
    )
    public interface DeleteProdCatalogAndStoreAssocVersatile {}

    /**
     * SCIPIO: Adds a ProductStoreCatalog association, first checking to make sure doesn't already exist (versatile)
     */
    @Service(
        name = "addProdCatalogStoreAssocVersatile",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/catalog/CatalogServices.xml",
        invoke = "addProdCatalogStoreAssocVersatile",
        description = "SCIPIO: Adds a ProductStoreCatalog association, first checking to make sure doesn't already exist (versatile)",
        auth = "true",
        implemented = {@Implements(service = "createProductStoreCatalog")}
    )
    public interface AddProdCatalogStoreAssocVersatile {}

    /**
     * SCIPIO: Create a ProductCategory and add as a top catalog category (if parentProductCategoryId is empty)              or as child of another category (if parentProductCategoryId is set) (versatile)
     */
    @Service(
        name = "createProductCategoryAndCatAssocVersatile",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "createProductCategoryAndCatAssocVersatile",
        description = "SCIPIO: Create a ProductCategory and add as a top catalog category (if parentProductCategoryId is empty) \n            or as child of another category (if parentProductCategoryId is set) (versatile)",
        auth = "true",
        implemented = {@Implements(service = "createProductCategory"), @Implements(service = "addProductCategoryToProdCatalog"), @Implements(service = "addProductCategoryToCategory"), @Implements(service = "replaceProductCategoryContentLocalizedSimpleTexts")},
        attributes = {
            @Attribute(name = "updateLocalizedTexts", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "productCategoryId", optional = "true"),
            @OverrideAttribute(name = "fromDate", optional = "true"),
            @OverrideAttribute(name = "prodCatalogId", optional = "true"),
            @OverrideAttribute(name = "prodCatalogCategoryTypeId", optional = "true"),
            @OverrideAttribute(name = "parentProductCategoryId", optional = "true")
        }
    )
    public interface CreateProductCategoryAndCatAssocVersatile {}

    /**
     * SCIPIO: Update a ProductCategory and its top association to catalog (if parentProductCategoryId is empty) or              its association to another category (if parentProductCategoryId is set) (versatile)
     */
    @Service(
        name = "updateProductCategoryAndCatAssocVersatile",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "updateProductCategoryAndCatAssocVersatile",
        description = "SCIPIO: Update a ProductCategory and its top association to catalog (if parentProductCategoryId is empty) or \n            its association to another category (if parentProductCategoryId is set) (versatile)",
        auth = "true",
        implemented = {@Implements(service = "updateProductCategory"), @Implements(service = "updateProductCategoryToProdCatalog"), @Implements(service = "updateProductCategoryToCategory"), @Implements(service = "replaceProductCategoryContentLocalizedSimpleTexts")},
        attributes = {
            @Attribute(name = "updateLocalizedTexts", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "productCategoryId", mode = "IN"),
            @OverrideAttribute(name = "fromDate", optional = "false"),
            @OverrideAttribute(name = "prodCatalogId", optional = "true"),
            @OverrideAttribute(name = "prodCatalogCategoryTypeId", optional = "true"),
            @OverrideAttribute(name = "parentProductCategoryId", optional = "true")
        }
    )
    public interface UpdateProductCategoryAndCatAssocVersatile {}

    /**
     * SCIPIO: Delete a ProductCategory catalog (if parentProductCategoryId empty) or              category (if parentProductCategoryId set) association (versatile)
     */
    @Service(
        name = "deleteProductCategoryCatAssocVersatile",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "deleteProductCategoryCatAssocVersatile",
        description = "SCIPIO: Delete a ProductCategory catalog (if parentProductCategoryId empty) or \n            category (if parentProductCategoryId set) association (versatile)",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "ProductCategory", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "ProdCatalogCategory", mode = "IN", include = "pk", optional = "true"),
            @EntityAttributes(entityName = "ProductCategoryRollup", mode = "IN", include = "pk", optional = "true")
        },
        attributes = {
            @Attribute(name = "deleteAssocMode", type = "String", mode = "IN", optional = "true", defaultValue = "remove", description = "(remove|expire, default: remove)")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "productCategoryId", optional = "false")
        }
    )
    public interface DeleteProductCategoryCatAssocVersatile {}

    /**
     * SCIPIO: Delete a ProductCategory record and various related records [WARN: may be dangerous - best-effort] (versatile)
     */
    @Service(
        name = "deleteProductCategoryAndRelatedVersatile",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "deleteProductCategoryAndRelatedVersatile",
        description = "SCIPIO: Delete a ProductCategory record and various related records [WARN: may be dangerous - best-effort] (versatile)",
        defaultEntityName = "ProductCategory",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "deleteParentAssocSelect", type = "String", mode = "IN", optional = "true", defaultValue = "all", description = "(none|expired|all, default: expired) Controls whether deletes all (default)\n                or only expired ProductCategoryRollup and ProdCatalogCategory records to this category's parents"),
            @Attribute(name = "deleteChildAssocSelect", type = "String", mode = "IN", optional = "true", defaultValue = "expired", description = "(none|expired|all, default: expired) If \"expired\" (default), removes children assoc records\n                such as ProductCategoryMember and ProductCategoryRollup that are expired; \n                if \"all\", removes all such records even if destructive.\n                WARN: \"all\" mode may leave products/categories as orphans!"),
            @Attribute(name = "deleteContentRecursive", type = "String", mode = "IN", optional = "true", defaultValue = "all", description = "(none|active|all, default: all) Controls whether and which Content records are deleted\n                recursively. If none, only the associations are deleted. \"active\" means non-expired.\n                WARN: This function only makes sense if the Content records were meant to be product-specific\n                    and not standalone; if standalone content is linked to the product then this may cause collateral damage!"),
            @Attribute(name = "deleteSpecialAssocSelect", type = "String", mode = "IN", optional = "true", defaultValue = "none", description = "(none|expired|all, default: expired) Special-purpose association deletion mode,\n                e.g. TaxAuthorityRateProduct and TaxAuthorityCategory\n                WARN: may currently be destructive (TODO: REVIEW) - \"expired\" is safe but only if no expired records are still needed.")
        }
    )
    public interface DeleteProductCategoryAndRelatedVersatile {}

    /**
     * SCIPIO: Delete a ProductCategory record (if no members) AND/OR a catalog/category association              based on wether parentProductCategoryId set (versatile)
     */
    @Service(
        name = "deleteProductCategoryAndCatAssocVersatile",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "deleteProductCategoryAndCatAssocVersatile",
        description = "SCIPIO: Delete a ProductCategory record (if no members) AND/OR a catalog/category association \n            based on wether parentProductCategoryId set (versatile)",
        auth = "true",
        implemented = {@Implements(service = "deleteProductCategoryCatAssocVersatile"), @Implements(service = "deleteProductCategoryAndRelatedVersatile")},
        attributes = {
            @Attribute(name = "deleteRecordAndRelated", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true", description = "If true, attempts to delete ProductCategory (using deleteProductCategoryAndRelatedVersatile); if false only deletes catalog or category association")
        }
    )
    public interface DeleteProductCategoryAndCatAssocVersatile {}

    /**
     * SCIPIO: Adds a category to catalog or category, attempting to prevent multiple associations (versatile)
     */
    @Service(
        name = "addProductCategoryCatAssocVersatile",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "addProductCategoryCatAssocVersatile",
        description = "SCIPIO: Adds a category to catalog or category, attempting to prevent multiple associations (versatile)",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "ProductCategory", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "ProdCatalogCategory", mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(entityName = "ProdCatalogCategory", mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(entityName = "ProductCategoryRollup", mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(entityName = "ProductCategoryRollup", mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "productCategoryId", optional = "false"),
            @OverrideAttribute(name = "fromDate", mode = "INOUT", optional = "true")
        }
    )
    public interface AddProductCategoryCatAssocVersatile {}

    /**
     * SCIPIO: Copies a ProductCategory assoc by creating a new assoc (catalog or category assoc), basically a special wrapper around addProductCategoryCatAssocVersatile (versatile)
     */
    @Service(
        name = "copyProductCategoryCatAssocVersatile",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "copyProductCategoryCatAssocVersatile",
        description = "SCIPIO: Copies a ProductCategory assoc by creating a new assoc (catalog or category assoc), basically a special wrapper around addProductCategoryCatAssocVersatile (versatile)",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "ProductCategory", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "ProdCatalogCategory", mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(entityName = "ProdCatalogCategory", mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(entityName = "ProductCategoryRollup", mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(entityName = "ProductCategoryRollup", mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "returnAssocFields", type = "String", mode = "IN", optional = "true", defaultValue = "false", description = "If true, the assoc fields for the new assoc will be outputted back to the\n                prefix-less fields in addition to the \"to_\"-prefixed ones (i.e., fromDate will\n                be output in both \"fromDate\" and \"to_fromDate\").\n                If false, the new assoc fields are output only to the \"to_\"-prefixed fields.\n                This can be used in events for switching the target easily after the update."),
            @Attribute(name = "to_fromDate", type = "Timestamp", mode = "INOUT", optional = "true"),
            @Attribute(name = "to_sequenceNum", type = "Long", mode = "INOUT", optional = "true"),
            @Attribute(name = "to_prodCatalogId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "to_prodCatalogCategoryTypeId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "to_parentProductCategoryId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "to_productCategoryId", type = "String", mode = "OUT", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "productCategoryId", optional = "false")
        }
    )
    public interface CopyProductCategoryCatAssocVersatile {}

    /**
     * SCIPIO: Moves a ProductCategory assoc by creating a new assoc (catalog or category assoc) and removing or expiring the original (versatile)
     */
    @Service(
        name = "moveProductCategoryCatAssocVersatile",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "moveProductCategoryCatAssocVersatile",
        description = "SCIPIO: Moves a ProductCategory assoc by creating a new assoc (catalog or category assoc) and removing or expiring the original (versatile)",
        auth = "true",
        implemented = {@Implements(service = "copyProductCategoryCatAssocVersatile")},
        attributes = {
            @Attribute(name = "deleteAssocMode", type = "String", mode = "IN", optional = "true", defaultValue = "remove", description = "(remove|expire, default: remove)")
        }
    )
    public interface MoveProductCategoryCatAssocVersatile {}

    /**
     * SCIPIO: Gets extended ProductCategory data (originally written for catalog tree edit form) (versatile)
     */
    @Service(
        name = "getProductCategoryExtendedDataVersatile",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/category/CategoryServices.xml",
        invoke = "getProductCategoryExtendedDataVersatile",
        description = "SCIPIO: Gets extended ProductCategory data (originally written for catalog tree edit form) (versatile)",
        auth = "true",
        implemented = {@Implements(service = "getProductCategoryContentLocalizedSimpleTextViews")},
        attributes = {
            @Attribute(name = "productCategory", type = "GenericValue", mode = "OUT")
        }
    )
    public interface GetProductCategoryExtendedDataVersatile {}

    /**
     * SCIPIO: Create a Product and add to category (specialized variant of createProductInCategory) (versatile)
     */
    @Service(
        name = "createProductAndCatAssocVersatile",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductServices.xml",
        invoke = "createProductAndCatAssocVersatile",
        description = "SCIPIO: Create a Product and add to category (specialized variant of createProductInCategory) (versatile)",
        auth = "true",
        implemented = {@Implements(service = "createProduct"), @Implements(service = "addProductToCategory"), @Implements(service = "replaceProductContentLocalizedSimpleTexts")},
        attributes = {
            @Attribute(name = "updateLocalizedTexts", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false")
        }
    )
    public interface CreateProductAndCatAssocVersatile {}

    /**
     * SCIPIO: Update a Product and category assoc (versatile)
     */
    @Service(
        name = "updateProductAndCatAssocVersatile",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductServices.xml",
        invoke = "updateProductAndCatAssocVersatile",
        description = "SCIPIO: Update a Product and category assoc (versatile)",
        auth = "true",
        implemented = {@Implements(service = "updateProduct"), @Implements(service = "updateProductToCategory"), @Implements(service = "replaceProductContentLocalizedSimpleTexts")},
        attributes = {
            @Attribute(name = "updateLocalizedTexts", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false")
        }
    )
    public interface UpdateProductAndCatAssocVersatile {}

    /**
     * SCIPIO: Delete a Product and category assoc (versatile)
     */
    @Service(
        name = "deleteProductCatAssocVersatile",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductServices.xml",
        invoke = "deleteProductCatAssocVersatile",
        description = "SCIPIO: Delete a Product and category assoc (versatile)",
        defaultEntityName = "Product",
        auth = "true",
        implemented = {@Implements(service = "removeProductFromCategory")},
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "deleteAssocMode", type = "String", mode = "IN", optional = "true", defaultValue = "remove", description = "(remove|expire, default: remove)")
        }
    )
    public interface DeleteProductCatAssocVersatile {}

    /**
     * SCIPIO: Delete a Product record and various related records [WARN: may be dangerous - best-effort] (versatile)
     */
    @Service(
        name = "deleteProductAndRelatedVersatile",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductServices.xml",
        invoke = "deleteProductAndRelatedVersatile",
        description = "SCIPIO: Delete a Product record and various related records [WARN: may be dangerous - best-effort] (versatile)",
        defaultEntityName = "Product",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "deleteParentAssocSelect", type = "String", mode = "IN", optional = "true", defaultValue = "all", description = "(none|expired|all, default: expired) Controls whether deletes all (default)\n                or only expired ProductCategoryMember records to this category's parents"),
            @Attribute(name = "deleteChildAssocSelect", type = "String", mode = "IN", optional = "true", defaultValue = "expired", description = "(none|expired|all, default: expired) If \"expired\" (default), removes children assoc records\n                that are expired; if \"all\", removes all such records even if destructive.\n                WARN: \"all\" mode may leave products/categories as orphans!"),
            @Attribute(name = "deleteContentRecursive", type = "String", mode = "IN", optional = "true", defaultValue = "all", description = "(none|active|all, default: all) Controls whether and which Content records are deleted\n                recursively. If none, only the associations are deleted. \"active\" means non-expired.\n                WARN: This function only makes sense if the Content records were meant to be product-specific\n                    and not standalone; if standalone content is linked to the product then this may cause collateral damage!"),
            @Attribute(name = "deleteAssocProductRecursive", type = "String", mode = "IN", optional = "true", defaultValue = "all", description = "(none|active|all, default: all) Controls whether and which associated \"to\"/\"child\" Products are deleted\n                recursively. If none, only the associations are deleted. \"active\" means non-expired.\n                WARN: Risk of collateral damage"),
            @Attribute(name = "deleteSpecialAssocSelect", type = "String", mode = "IN", optional = "true", defaultValue = "none", description = "(none|expired|all, default: expired) Special-purpose association deletion mode\n                WARN: may currently be destructive (TODO: REVIEW) - \"expired\" is safe but only if no expired records are still needed.")
        }
    )
    public interface DeleteProductAndRelatedVersatile {}

    /**
     * SCIPIO: Delete a Product record (if no members) and category assoc (versatile)
     */
    @Service(
        name = "deleteProductAndCatAssocVersatile",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductServices.xml",
        invoke = "deleteProductAndCatAssocVersatile",
        description = "SCIPIO: Delete a Product record (if no members) and category assoc (versatile)",
        auth = "true",
        implemented = {@Implements(service = "deleteProductCatAssocVersatile"), @Implements(service = "deleteProductAndRelatedVersatile")},
        attributes = {
            @Attribute(name = "deleteRecordAndRelated", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true", description = "If true, attempts to delete Product (using deleteProductAndRelatedVersatile); if false only deletes catalog or category association")
        }
    )
    public interface DeleteProductAndCatAssocVersatile {}

    /**
     * SCIPIO: Adds a category to category, attempting to prevent multiple associations (versatile)
     */
    @Service(
        name = "addProductCatAssocVersatile",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductServices.xml",
        invoke = "addProductCatAssocVersatile",
        description = "SCIPIO: Adds a category to category, attempting to prevent multiple associations (versatile)",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "Product", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "ProductCategoryMember", mode = "INOUT", include = "pk"),
            @EntityAttributes(entityName = "ProductCategoryMember", mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", mode = "INOUT", optional = "true")
        }
    )
    public interface AddProductCatAssocVersatile {}

    /**
     * SCIPIO: Copies a Product assoc by creating a new assoc, basically a special wrapper around addProductCatAssocVersatile (versatile)
     */
    @Service(
        name = "copyProductCatAssocVersatile",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductServices.xml",
        invoke = "copyProductCatAssocVersatile",
        description = "SCIPIO: Copies a Product assoc by creating a new assoc, basically a special wrapper around addProductCatAssocVersatile (versatile)",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "Product", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "ProductCategoryMember", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "ProductCategoryMember", mode = "OUT", include = "pk", optional = "true")
        },
        attributes = {
            @Attribute(name = "returnAssocFields", type = "String", mode = "IN", optional = "true", defaultValue = "false", description = "If true, the assoc fields for the new assoc will be outputted back to the\n                prefix-less fields in addition to the \"to_\"-prefixed ones (i.e., fromDate will\n                be output in both \"fromDate\" and \"to_fromDate\").\n                If false, the new assoc fields are output only to the \"to_\"-prefixed fields.\n                This can be used in events for switching the target easily after the update."),
            @Attribute(name = "to_productCategoryId", type = "String", mode = "INOUT"),
            @Attribute(name = "to_fromDate", type = "Timestamp", mode = "INOUT"),
            @Attribute(name = "to_sequenceNum", type = "Long", mode = "INOUT"),
            @Attribute(name = "to_productId", type = "String", mode = "OUT")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "productId", optional = "false")
        }
    )
    public interface CopyProductCatAssocVersatile {}

    /**
     * SCIPIO: Moves a Product assoc by creating a new assoc and removing or expiring the original (versatile)
     */
    @Service(
        name = "moveProductCatAssocVersatile",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductServices.xml",
        invoke = "moveProductCatAssocVersatile",
        description = "SCIPIO: Moves a Product assoc by creating a new assoc and removing or expiring the original (versatile)",
        auth = "true",
        implemented = {@Implements(service = "copyProductCatAssocVersatile")},
        attributes = {
            @Attribute(name = "deleteAssocMode", type = "String", mode = "IN", optional = "true", defaultValue = "remove", description = "(remove|expire, default: remove)")
        }
    )
    public interface MoveProductCatAssocVersatile {}

    /**
     * SCIPIO: Gets extended Product data (originally written for catalog tree edit form) (versatile)
     */
    @Service(
        name = "getProductExtendedDataVersatile",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/product/ProductServices.xml",
        invoke = "getProductExtendedDataVersatile",
        description = "SCIPIO: Gets extended Product data (originally written for catalog tree edit form) (versatile)",
        auth = "true",
        implemented = {@Implements(service = "getProductContentLocalizedSimpleTextViews")},
        attributes = {
            @Attribute(name = "product", type = "GenericValue", mode = "OUT")
        }
    )
    public interface GetProductExtendedDataVersatile {}

    /**
     * SCIPIO: Builds a tree containing catalogs, categories and products using catalogs as starting point
     */
    @Service(
        name = "buildCatalogTree",
        engine = "java",
        location = "com.ilscipio.scipio.product.category.CategoryServices",
        invoke = "buildCatalogTree",
        description = "SCIPIO: Builds a tree containing catalogs, categories and products using catalogs as starting point",
        defaultEntityName = "ProdCatalog",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "library", type = "String", mode = "IN", optional = "true", defaultValue = "jsTree"),
            @Attribute(name = "mode", type = "String", mode = "IN", optional = "true", defaultValue = "full"),
            @Attribute(name = "state", type = "Map", mode = "IN", optional = "true", description = "Map of state attributes for top node: opened, selected, disabled. (added 2017-10-11)"),
            @Attribute(name = "categoryStates", type = "Map", mode = "IN", optional = "true", description = "Map of categoryIds to maps of state attributes (added 2017-10-11).\n                WARN: can't specify specific path for many-parents"),
            @Attribute(name = "useCategoryCache", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "useProductCache", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "includeEntityData", type = "Map", mode = "IN", optional = "true", description = "Map of names to entity-like field names to include in each tree item as \"[name]Entity\" entries (e.g. \"productEntity\").\n                Available names: prodCatalog, productStoreCatalog, productCategory, productCategoryRollup, prodCatalogCategory, product, productCategoryMember\n                Map values may be:\n                * Boolean or boolean as String - true means include all fields, false none\n                * Collection of field names to include\n                * Map of options. Supported keys:\n                  * use - boolean, whether to use\n                  * inclFields - Collection of field names to include\n                  * exclFields - Collection of field names to exclude\n            "),
            @Attribute(name = "includeAllEntityData", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If true, simply includes all available entity data; includeEntityData can still be set to specialize selection."),
            @Attribute(name = "maxProductsPerCat", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "includeEmptyTop", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If false (default), empty catalog or top node is omitted"),
            @Attribute(name = "productStoreCatalog", type = "GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "categoryEntityOutMap", type = "Map", mode = "INOUT", optional = "true", description = "Map of categoryIds to ProductCategory GenericValues; if non-null, \n                all categories found are added to this map (needed to avoid performing expensive traversal twice)"),
            @Attribute(name = "treeList", type = "List", mode = "OUT")
        }
    )
    public interface BuildCatalogTree {}

    /**
     * SCIPIO: Discontinues (manually disables) a product and updates solr.
     */
    @Service(
        name = "discontinueProduct",
        engine = "java",
        location = "com.ilscipio.scipio.product.product.ProductServices",
        invoke = "setProductToSalesDiscontinued",
        description = "SCIPIO: Discontinues (manually disables) a product and updates solr.",
        auth = "true",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN")
        }
    )
    public interface DiscontinueProduct {}

    /**
     * SCIPIO: Re-generates alternative urls for product based on the ruleset outlined in SeoConfig.xml [core only - no perm check]
     */
    @Service(
        name = "generateProductAlternativeUrlsCore",
        engine = "java",
        location = "com.ilscipio.scipio.product.seo.SeoCatalogServices",
        invoke = "generateProductAlternativeUrls",
        description = "SCIPIO: Re-generates alternative urls for product based on the ruleset outlined in SeoConfig.xml [core only - no perm check]",
        auth = "true",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "product", type = "GenericEntity", mode = "IN", optional = "true"),
            @Attribute(name = "doChildProducts", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If true and virtual, also updates the variants (NOTE: parent and children done in the same transaction)"),
            @Attribute(name = "includeVariant", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true", description = "Does NOT apply self, only consulted if doChildProducts true (need mostly for consistency with callers)"),
            @Attribute(name = "skipProductIds", type = "Collection", mode = "IN", optional = "true"),
            @Attribute(name = "replaceExisting", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true"),
            @Attribute(name = "removeOldLocales", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true"),
            @Attribute(name = "genFixedIds", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "Generate fixed IDs with pattern \"${idTrim}-ALT${localeStrUp}\" (idTrim is id trimmed so total length under 20), mainly for demo data generation - high risk of failure if set to true in any other case"),
            @Attribute(name = "fixedIdPat", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "webSiteId", type = "String", mode = "IN", optional = "true", description = "Website for URL generation configuration via SeoConfig.xml"),
            @Attribute(name = "mainContentId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "numUpdated", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "numSkipped", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "numError", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "visitedProductIds", type = "Set", mode = "OUT", optional = "true")
        }
    )
    public interface GenerateProductAlternativeUrlsCore {}

    /**
     * SCIPIO: Re-generates alternative urls for product based on the ruleset outlined in SeoConfig.xml             WARN/TODO?: this service does not run for variant or other child products
     */
    @Service(
        name = "generateProductAlternativeUrls",
        engine = "java",
        location = "com.ilscipio.scipio.product.seo.SeoCatalogServices",
        invoke = "generateProductAlternativeUrls",
        description = "SCIPIO: Re-generates alternative urls for product based on the ruleset outlined in SeoConfig.xml\n            WARN/TODO?: this service does not run for variant or other child products",
        auth = "true",
        implemented = {@Implements(service = "generateProductAlternativeUrlsCore")},
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "UPDATE")
    )
    public interface GenerateProductAlternativeUrls {}

    /**
     * SCIPIO: Re-generates alternative urls for category based on the ruleset outlined in SeoConfig.xml [core only - no perm check]
     */
    @Service(
        name = "generateProductCategoryAlternativeUrlsCore",
        engine = "java",
        location = "com.ilscipio.scipio.product.seo.SeoCatalogServices",
        invoke = "generateProductCategoryAlternativeUrls",
        description = "SCIPIO: Re-generates alternative urls for category based on the ruleset outlined in SeoConfig.xml [core only - no perm check]",
        auth = "true",
        attributes = {
            @Attribute(name = "productCategoryId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productCategory", type = "GenericEntity", mode = "IN", optional = "true"),
            @Attribute(name = "replaceExisting", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true"),
            @Attribute(name = "removeOldLocales", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true"),
            @Attribute(name = "genFixedIds", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "Generate fixed IDs with pattern \"${idTrim}-ALT${localeStrUp}\" (idTrim is id trimmed so total length under 20), mainly for demo data generation - high risk of failure if set to true in any other case"),
            @Attribute(name = "fixedIdPat", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "webSiteId", type = "String", mode = "IN", optional = "true", description = "Website for URL generation configuration via SeoConfig.xml"),
            @Attribute(name = "mainContentId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "categoryUpdated", type = "Boolean", mode = "OUT", optional = "true")
        }
    )
    public interface GenerateProductCategoryAlternativeUrlsCore {}

    /**
     * SCIPIO: Re-generates alternative urls for category based on the ruleset outlined in SeoConfig.xml
     */
    @Service(
        name = "generateProductCategoryAlternativeUrls",
        engine = "java",
        location = "com.ilscipio.scipio.product.seo.SeoCatalogServices",
        invoke = "generateProductCategoryAlternativeUrls",
        description = "SCIPIO: Re-generates alternative urls for category based on the ruleset outlined in SeoConfig.xml",
        auth = "true",
        implemented = {@Implements(service = "generateProductCategoryAlternativeUrlsCore")},
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "UPDATE")
    )
    public interface GenerateProductCategoryAlternativeUrls {}

    @Service(
        name = "websiteAlternativeUrlsCatalogInterface",
        engine = "interface",
        attributes = {
            @Attribute(name = "webSiteId", type = "String", mode = "IN", optional = "true", description = "May pass webSiteId OR productStoreId"),
            @Attribute(name = "productStoreId", type = "String", mode = "IN", optional = "true", description = "May pass webSiteId OR productStoreId"),
            @Attribute(name = "prodCatalogId", type = "String", mode = "IN", optional = "true", description = "Limits to a specific catalog"),
            @Attribute(name = "prodCatalogIdList", type = "List", mode = "IN", optional = "true", description = "Limits to specific catalogs")
        }
    )
    public interface WebsiteAlternativeUrlsCatalogInterface {}

    /**
     * SCIPIO: Re-generates alternative urls for store/website based on ruleset outlined in SeoConfig.xml - replaces old generateMissingSeoUrlForWebsite service             FIXME?: this can include variant products, but does not yet recognize other ProductAssoc types
     */
    @Service(
        name = "generateWebsiteAlternativeUrls",
        engine = "java",
        location = "com.ilscipio.scipio.product.seo.SeoCatalogServices",
        invoke = "generateWebsiteAlternativeUrls",
        description = "SCIPIO: Re-generates alternative urls for store/website based on ruleset outlined in SeoConfig.xml - replaces old generateMissingSeoUrlForWebsite service\n            FIXME?: this can include variant products, but does not yet recognize other ProductAssoc types",
        auth = "true",
        transactionTimeout = "72000",
        implemented = {@Implements(service = "websiteAlternativeUrlsCatalogInterface")},
        attributes = {
            @Attribute(name = "typeGenerate", type = "List", mode = "IN", optional = "true", defaultValue = "[all]", description = "Currently supports: product, category, all."),
            @Attribute(name = "replaceExisting", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true"),
            @Attribute(name = "removeOldLocales", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true"),
            @Attribute(name = "includeVariant", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true"),
            @Attribute(name = "preventDuplicates", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true", description = "Tries to prevent regenerating the same URLs for the same categories and products more than once"),
            @Attribute(name = "useCache", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "May be slightly faster iterating if true, but not recommended"),
            @Attribute(name = "genFixedIds", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "Generate fixed IDs with pattern \"${id}-ALT\", mainly for demo data generation - high risk of failure if set to true in any other case"),
            @Attribute(name = "prodFixedIdPat", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "catFixedIdPat", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "sepTrans", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "UPDATE")
    )
    public interface GenerateWebsiteAlternativeUrls {}

    /**
     * SCIPIO: Re-generates alternative urls for ALL stores/websites based on ruleset outlined in SeoConfig.xml
     */
    @Service(
        name = "generateAllAlternativeUrls",
        engine = "java",
        location = "com.ilscipio.scipio.product.seo.SeoCatalogServices",
        invoke = "generateAllAlternativeUrls",
        description = "SCIPIO: Re-generates alternative urls for ALL stores/websites based on ruleset outlined in SeoConfig.xml",
        auth = "true",
        transactionTimeout = "72000",
        attributes = {
            @Attribute(name = "typeGenerate", type = "List", mode = "IN", optional = "true", defaultValue = "[all]", description = "Currently supports: product, category, all."),
            @Attribute(name = "replaceExisting", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true"),
            @Attribute(name = "removeOldLocales", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true"),
            @Attribute(name = "includeVariant", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true"),
            @Attribute(name = "useCache", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "May be slightly faster iterating if true, but not recommended"),
            @Attribute(name = "genFixedIds", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "Generate fixed IDs with pattern \"${id}-ALT\", mainly for demo data generation - high risk of failure if set to true in any other case"),
            @Attribute(name = "prodFixedIdPat", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "catFixedIdPat", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "sepTrans", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "UPDATE")
    )
    public interface GenerateAllAlternativeUrls {}

    /**
     * SCIPIO: Removes alternative urls for product [core only - no perm check]
     */
    @Service(
        name = "removeProductAlternativeUrlsCore",
        engine = "java",
        location = "com.ilscipio.scipio.product.seo.SeoCatalogServices",
        invoke = "removeProductAlternativeUrls",
        description = "SCIPIO: Removes alternative urls for product [core only - no perm check]",
        auth = "true",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "product", type = "GenericEntity", mode = "IN", optional = "true"),
            @Attribute(name = "doChildProducts", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If true and virtual, also updates the variants (NOTE: parent and children done in the same transaction)"),
            @Attribute(name = "includeVariant", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true", description = "Does NOT apply self, only consulted if doChildProducts true (need mostly for consistency with callers)"),
            @Attribute(name = "skipProductIds", type = "Collection", mode = "IN", optional = "true"),
            @Attribute(name = "numUpdated", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "numSkipped", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "numError", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "visitedProductIds", type = "Set", mode = "OUT", optional = "true")
        }
    )
    public interface RemoveProductAlternativeUrlsCore {}

    /**
     * SCIPIO: Removes alternative urls for product
     */
    @Service(
        name = "removeProductAlternativeUrls",
        engine = "java",
        location = "com.ilscipio.scipio.product.seo.SeoCatalogServices",
        invoke = "removeProductAlternativeUrls",
        description = "SCIPIO: Removes alternative urls for product",
        auth = "true",
        implemented = {@Implements(service = "removeProductAlternativeUrlsCore")},
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "UPDATE")
    )
    public interface RemoveProductAlternativeUrls {}

    /**
     * SCIPIO: Remove alternative urls for category [core only - no perm check]
     */
    @Service(
        name = "removeProductCategoryAlternativeUrlsCore",
        engine = "java",
        location = "com.ilscipio.scipio.product.seo.SeoCatalogServices",
        invoke = "removeProductCategoryAlternativeUrls",
        description = "SCIPIO: Remove alternative urls for category [core only - no perm check]",
        auth = "true",
        attributes = {
            @Attribute(name = "productCategoryId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productCategory", type = "GenericEntity", mode = "IN", optional = "true"),
            @Attribute(name = "categoryUpdated", type = "Boolean", mode = "OUT", optional = "true")
        }
    )
    public interface RemoveProductCategoryAlternativeUrlsCore {}

    /**
     * SCIPIO: Remove alternative urls for category
     */
    @Service(
        name = "removeProductCategoryAlternativeUrls",
        engine = "java",
        location = "com.ilscipio.scipio.product.seo.SeoCatalogServices",
        invoke = "removeProductCategoryAlternativeUrls",
        description = "SCIPIO: Remove alternative urls for category",
        auth = "true",
        implemented = {@Implements(service = "removeProductCategoryAlternativeUrlsCore")},
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "UPDATE")
    )
    public interface RemoveProductCategoryAlternativeUrls {}

    /**
     * SCIPIO: Removes alternative URLs for the website             FIXME?: this can include variant products, but does not yet recognize other ProductAssoc types
     */
    @Service(
        name = "removeWebsiteAlternativeUrls",
        engine = "java",
        location = "com.ilscipio.scipio.product.seo.SeoCatalogServices",
        invoke = "removeWebsiteAlternativeUrls",
        description = "SCIPIO: Removes alternative URLs for the website\n            FIXME?: this can include variant products, but does not yet recognize other ProductAssoc types",
        auth = "true",
        transactionTimeout = "72000",
        implemented = {@Implements(service = "websiteAlternativeUrlsCatalogInterface")},
        attributes = {
            @Attribute(name = "targetTypes", type = "List", mode = "IN", optional = "true", defaultValue = "[all]", description = "Currently supports: product, category, all."),
            @Attribute(name = "includeVariant", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true"),
            @Attribute(name = "sepTrans", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "UPDATE")
    )
    public interface RemoveWebsiteAlternativeUrls {}

    /**
     * SCIPIO: Removes alternative urls from all products and categories
     */
    @Service(
        name = "removeAllAlternativeUrls",
        engine = "java",
        location = "com.ilscipio.scipio.product.seo.SeoCatalogServices",
        invoke = "removeAllAlternativeUrls",
        description = "SCIPIO: Removes alternative urls from all products and categories",
        auth = "true",
        transactionTimeout = "72000",
        attributes = {
            @Attribute(name = "targetTypes", type = "List", mode = "IN", optional = "true", defaultValue = "[all]", description = "Currently supports: product, category, all."),
            @Attribute(name = "includeVariant", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true"),
            @Attribute(name = "sepTrans", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "UPDATE")
    )
    public interface RemoveAllAlternativeUrls {}

    /**
     * Creates sitemaps in filesystem using product/category alternative URLs, for WebSite, using configurations from sitemaps.properties
     */
    @Service(
        name = "generateWebsiteAlternativeUrlSitemapFiles",
        engine = "java",
        location = "com.ilscipio.scipio.product.seo.sitemap.SitemapServices",
        invoke = "generateWebsiteAlternativeUrlSitemapFiles",
        description = "Creates sitemaps in filesystem using product/category alternative URLs, for WebSite, using configurations from sitemaps.properties",
        transactionTimeout = "72000",
        attributes = {
            @Attribute(name = "webSiteId", type = "String", mode = "IN"),
            @Attribute(name = "useCache", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "May be slightly faster iterating if true, but not recommended")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "UPDATE")
    )
    public interface GenerateWebsiteAlternativeUrlSitemapFiles {}

    /**
     * Creates sitemaps in filesystem using product/category alternative URLs, for all WebSites that have it enabled, using configurations from sitemaps.properties
     */
    @Service(
        name = "generateAllAlternativeUrlSitemapFiles",
        engine = "java",
        location = "com.ilscipio.scipio.product.seo.sitemap.SitemapServices",
        invoke = "generateAllAlternativeUrlSitemapFiles",
        description = "Creates sitemaps in filesystem using product/category alternative URLs, for all WebSites that have it enabled, using configurations from sitemaps.properties",
        transactionTimeout = "72000",
        attributes = {
            @Attribute(name = "useCache", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "May be slightly faster iterating if true, but not recommended")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "UPDATE")
    )
    public interface GenerateAllAlternativeUrlSitemapFiles {}

    /**
     * Traversal options for alt URL entity XML services
     */
    @Service(
        name = "exportAlternativeUrlsEntityXmlOptionsInterface",
        engine = "interface",
        description = "Traversal options for alt URL entity XML services",
        attributes = {
            @Attribute(name = "typeExport", type = "List", mode = "IN", optional = "true", defaultValue = "[all]", description = "Currently supports: product, category, all."),
            @Attribute(name = "includeVariant", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true"),
            @Attribute(name = "preventDuplicates", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true", description = "Tries to prevent regenerating the same URLs for the same categories and products more than one"),
            @Attribute(name = "useCache", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "May be slightly faster iterating if true, but not recommended"),
            @Attribute(name = "linePrefix", type = "String", mode = "IN", optional = "true", description = "String prefix for each line, typically an indent"),
            @Attribute(name = "recordGrouping", type = "String", mode = "IN", optional = "true", description = "Supported values: NONE/MAJOR_OBJECT (default), ENTITY_TYPE")
        }
    )
    public interface ExportAlternativeUrlsEntityXmlOptionsInterface {}

    /**
     * Output options for alt URL entity XML services
     */
    @Service(
        name = "exportAlternativeUrlsEntityXmlWriterInterface",
        engine = "interface",
        description = "Output options for alt URL entity XML services",
        attributes = {
            @Attribute(name = "asString", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If true, will return result as a string in outString in addition\n                to outputting to the passed outWriter if any was passed (if none, also returns a new StringWriter)"),
            @Attribute(name = "outFlush", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true"),
            @Attribute(name = "outWriter", type = "java.io.Writer", mode = "INOUT", optional = "true", description = "Writer to output the formatted XML records to"),
            @Attribute(name = "outString", type = "String", mode = "OUT", optional = "true", description = "If this is set to \"Y\" or \"true\" on input and the outWriter is either\n                not set or a StringWriter, this will contain the output as a string upon success.", allowHtml = "any")
        }
    )
    public interface ExportAlternativeUrlsEntityXmlWriterInterface {}

    /**
     * Exports alternative urls for given website to entity XML data out writer
     */
    @Service(
        name = "exportWebsiteAlternativeUrlsEntityXml",
        engine = "java",
        location = "com.ilscipio.scipio.product.seo.SeoCatalogServices",
        invoke = "exportWebsiteAlternativeUrlsEntityXml",
        description = "Exports alternative urls for given website to entity XML data out writer",
        auth = "true",
        transactionTimeout = "72000",
        implemented = {@Implements(service = "exportAlternativeUrlsEntityXmlOptionsInterface"), @Implements(service = "exportAlternativeUrlsEntityXmlWriterInterface"), @Implements(service = "websiteAlternativeUrlsCatalogInterface")},
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "VIEW")
    )
    public interface ExportWebsiteAlternativeUrlsEntityXml {}

    /**
     * Export all alternative URLs into a single output to entity XML data out writer
     */
    @Service(
        name = "exportAllAlternativeUrlsEntityXml",
        engine = "java",
        location = "com.ilscipio.scipio.product.seo.SeoCatalogServices",
        invoke = "exportAllAlternativeUrlsEntityXml",
        description = "Export all alternative URLs into a single output to entity XML data out writer",
        transactionTimeout = "72000",
        implemented = {@Implements(service = "exportAlternativeUrlsEntityXmlOptionsInterface"), @Implements(service = "exportAlternativeUrlsEntityXmlWriterInterface")},
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "VIEW")
    )
    public interface ExportAllAlternativeUrlsEntityXml {}

    /**
     * Exports alternative urls for given website or all websites to entity XML data file
     */
    @Service(
        name = "exportWebsiteAlternativeUrlsEntityXmlFile",
        engine = "java",
        location = "com.ilscipio.scipio.product.seo.SeoCatalogServices",
        invoke = "exportWebsiteAlternativeUrlsEntityXmlFile",
        description = "Exports alternative urls for given website or all websites to entity XML data file",
        transactionTimeout = "72000",
        implemented = {@Implements(service = "exportAlternativeUrlsEntityXmlOptionsInterface"), @Implements(service = "websiteAlternativeUrlsCatalogInterface")},
        attributes = {
            @Attribute(name = "outFile", type = "String", mode = "IN", description = "Should begin with component://"),
            @Attribute(name = "templateType", type = "String", mode = "IN", description = "Supported: comment-delimited (TODO: ftl)"),
            @Attribute(name = "dataBeginMarker", type = "String", mode = "IN", optional = "true", description = "All file contents before this marker is copied from the template to the outFile", allowHtml = "any"),
            @Attribute(name = "templateFile", type = "String", mode = "IN", optional = "true", description = "Should begin with component://")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "VIEW")
    )
    public interface ExportWebsiteAlternativeUrlsEntityXmlFile {}

    /**
     * Exports alternative urls for given website or all websites to entity XML data file,             from config in seo-urls.properties files
     */
    @Service(
        name = "exportAlternativeUrlsEntityXmlFileFromConfig",
        engine = "java",
        location = "com.ilscipio.scipio.product.seo.SeoCatalogServices",
        invoke = "exportAlternativeUrlsEntityXmlFileFromConfig",
        description = "Exports alternative urls for given website or all websites to entity XML data file,\n            from config in seo-urls.properties files",
        transactionTimeout = "72000",
        attributes = {
            @Attribute(name = "configName", type = "String", mode = "IN", optional = "true", description = "Name of config in seo-urls.properties, in format: seourl.datafile.[configName].[params]"),
            @Attribute(name = "configNameList", type = "List", mode = "IN", optional = "true", description = "List of config names in seo-urls.properties, in format: seourl.datafile.[configName].[params]")
        },
        permissionService = @PermissionService(service = "productGenericPermission", mainAction = "VIEW")
    )
    public interface ExportAlternativeUrlsEntityXmlFileFromConfig {}

    /**
     * Pre-caches ProductContentWrapper records for all found products
     */
    @Service(
        name = "precacheProductContentWrapper",
        engine = "java",
        location = "com.ilscipio.scipio.product.product.ProductServices$PrecacheProductContentWrapper",
        invoke = "exec",
        description = "Pre-caches ProductContentWrapper records for all found products",
        attributes = {
            @Attribute(name = "productContentTypeIdList", type = "List", mode = "IN"),
            @Attribute(name = "partyIdList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "roleTypeIdList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "encoderTypeList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "mimeTypeIdList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "localeList", type = "List", mode = "IN", optional = "true")
        }
    )
    public interface PrecacheProductContentWrapper {}

    /**
     * Creates or updates simple text content for alternate locale for product             - supports either explicit contentId or relying on localeString to find record to update             - WARN: For high-level operation, use replaceContentLocalizedSimpleTexts
     */
    @Service(
        name = "createUpdateProductSimpleTextContentForAlternateLocale",
        engine = "java",
        location = "com.ilscipio.scipio.product.product.ProductServices$CreateUpdateProductSimpleTextContentForAlternateLocale",
        invoke = "exec",
        description = "Creates or updates simple text content for alternate locale for product\n            - supports either explicit contentId or relying on localeString to find record to update\n            - WARN: For high-level operation, use replaceContentLocalizedSimpleTexts",
        implemented = {@Implements(service = "createUpdateSimpleTextContentForAlternateLocale")},
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "productContentTypeId", type = "String", mode = "IN"),
            @Attribute(name = "productContentFromDate", type = "Timestamp", mode = "INOUT", optional = "true")
        }
    )
    public interface CreateUpdateProductSimpleTextContentForAlternateLocale {}

    /**
     * Creates or updates simple text content for alternate locale for product category             - supports either explicit contentId or relying on localeString to find record to update             - WARN: For high-level operation, use replaceContentLocalizedSimpleTexts
     */
    @Service(
        name = "createUpdateProductCategorySimpleTextContentForAlternateLocale",
        engine = "java",
        location = "com.ilscipio.scipio.product.category.CategoryServices$CreateUpdateProductCategorySimpleTextContentForAlternateLocale",
        invoke = "exec",
        description = "Creates or updates simple text content for alternate locale for product category\n            - supports either explicit contentId or relying on localeString to find record to update\n            - WARN: For high-level operation, use replaceContentLocalizedSimpleTexts",
        implemented = {@Implements(service = "createUpdateSimpleTextContentForAlternateLocale")},
        attributes = {
            @Attribute(name = "productCategoryId", type = "String", mode = "IN"),
            @Attribute(name = "prodCatContentTypeId", type = "String", mode = "IN"),
            @Attribute(name = "prodCatContentFromDate", type = "Timestamp", mode = "INOUT", optional = "true")
        }
    )
    public interface CreateUpdateProductCategorySimpleTextContentForAlternateLocale {}

}

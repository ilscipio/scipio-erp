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
public class ImageServices {

    @Service(
        name = "imageFileScaleCommon",
        engine = "interface",
        attributes = {
            @Attribute(name = "imageOrigPath", type = "String", mode = "IN", optional = "true", description = "Full path of original image in filesystem as input (alternative to imageOrigUrl; if neither specified, auto-determines an original filename under imageServerPath); also supports component:// and file:// prefixes"),
            @Attribute(name = "imageOrigUrl", type = "String", mode = "IN", optional = "true", description = "URL of image relative to server root of original image in filesystem as input; WARN: 2017-07-04: MUST start with imageUrlPrefix else error; no other mount-points supported (alternative to imageOrigPath; if neither specified, auto-determines an original filename under imageServerPath)"),
            @Attribute(name = "imageOrigFn", type = "String", mode = "IN", optional = "true", description = "Original filename (no directories) of the image - required only if neither imageOrigPath nor imageOrigUrl specified"),
            @Attribute(name = "imageOrigFnFmt", type = "String", mode = "IN", optional = "true", description = "Image filename format string for original only, relative to imageServerPath/imageUrlPrefix, no extension; only useful if copyOrig==true or if imageOrigPath/imageOrigUrl are omitted (default: same as imageFnFmt)"),
            @Attribute(name = "imageServerPath", type = "String", mode = "IN", optional = "true", description = "Full filesystem path of base server image, parameterized with ${tenantId} (default: uses image.server.path / catalog.properties); also supports component:// and file:// prefixes"),
            @Attribute(name = "imageUrlPrefix", type = "String", mode = "IN", optional = "true", description = "URL prefix for generated images, parameterized with ${tenantId} (default: uses image.url.prefix / catalog.properties)"),
            @Attribute(name = "imageFnFmt", type = "String", mode = "IN", optional = "true", description = "Image filename format string, relative to imageServerPath/imageUrlPrefix, no extension, parameterized with ${sizetype} (or ${type}) and product fields (default: uses image.filename.format OR image.filename.additionalviewsize.format / catalog.properties)"),
            @Attribute(name = "imagePathArgs", type = "Map", mode = "IN", optional = "true", description = "Additional args for parameterized paths"),
            @Attribute(name = "imageProfile", type = "Object", mode = "IN", optional = "true", description = "Image profile, now generally required (name or org.ofbiz.common.image.ImageProfile)"),
            @Attribute(name = "imagePropXmlPath", type = "String", mode = "IN", optional = "true", description = "Path to ImageProperties.xml file containing size types, from ofbiz home root"),
            @Attribute(name = "sizeTypeList", type = "Collection", mode = "IN", optional = "true", description = "List of size types to generate and return (default: all types in file)"),
            @Attribute(name = "copyOrig", type = "Boolean", mode = "IN", optional = "true", description = "If true, also creates copy of the original under the size type \"original\" (default: false)"),
            @Attribute(name = "deleteOld", type = "Boolean", mode = "IN", optional = "true", description = "[TODO: NOT IMPLEMENTED] If true, also deletes old files in target directory (default: false)"),
            @Attribute(name = "imageWriteOptions", type = "Map", mode = "IN", optional = "true"),
            @Attribute(name = "scalingOptions", type = "Map", mode = "IN", optional = "true", description = "Scaling options, notably the entry: scalerName (algorithm or library name)"),
            @Attribute(name = "imageUrlMap", type = "Map", mode = "OUT", optional = "true", description = "Map of size types to URLs (relative to server root, with imageUrlPrefix); if copyOrig==true, also contains \"original\" (UNLESS found to already exist / same as input)\n                NOTE: Unlike stock ofbiz functions, this returns ALL generated. Use productSizeTypeList to iterate the common size types."),
            @Attribute(name = "imageInfoMap", type = "Map", mode = "OUT", optional = "true", description = "Map of maps describing url, width, height and variantInfo for each sizeType; also contains \"original\" which contains copyOrig boolean"),
            @Attribute(name = "bufferedImage", type = "java.awt.image.BufferedImage", mode = "OUT", optional = "true", description = "Original image contents, for reuse"),
            @Attribute(name = "successCount", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "failCount", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "skipCount", type = "Integer", mode = "OUT", optional = "true")
        }
    )
    public interface ImageFileScaleCommon {}

    /**
     * Scales a product image file according to size types in product config ImageProperties.xml and using filename formats from catalog.properties
     */
    @Service(
        name = "productImageFileScaleInAllSize",
        engine = "java",
        location = "com.ilscipio.scipio.product.image.ProductImageServices",
        invoke = "productImageFileScaleInAllSize",
        description = "Scales a product image file according to size types in product config ImageProperties.xml and using filename formats from catalog.properties",
        implemented = {@Implements(service = "imageFileScaleCommon")},
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN", description = "productId"),
            @Attribute(name = "imageViewType", type = "com.ilscipio.scipio.product.image.ProductImageViewType", mode = "IN", optional = "true", description = "Full instance alternative to viewType/viewNumber parameters (NOTE: imageViewType should be \"original\" viewSize only)"),
            @Attribute(name = "viewType", type = "String", mode = "IN", optional = "true", description = "main|additional"),
            @Attribute(name = "viewNumber", type = "Object", mode = "IN", optional = "true", description = "For additional (String or Integer), a number between 1-4 for stock records; 0 for main viewType"),
            @Attribute(name = "imageVariantConfig", type = "org.ofbiz.common.image.ImageVariantConfig", mode = "IN", optional = "true"),
            @Attribute(name = "productSizeTypeList", type = "List", mode = "OUT", optional = "true", description = "The list of typical product size types: [small, medium, large, detail]")
        }
    )
    public interface ProductImageFileScaleInAllSize {}

    /**
     * Automatically rescales one or more product images for a product (does NOT consult parent/virtual products)
     */
    @Service(
        name = "productImageAutoRescale",
        engine = "java",
        location = "com.ilscipio.scipio.product.image.ProductImageServices",
        invoke = "productImageAutoRescale",
        description = "Automatically rescales one or more product images for a product (does NOT consult parent/virtual products)",
        transactionTimeout = "1800",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "product", type = "GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "allImages", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If true, automatically tries to apply to all the known associated images or all productContentTypeId"),
            @Attribute(name = "productContentTypeId", type = "String", mode = "IN", optional = "true", description = "productContentTypeId of the ProductContent for the image - for main product image use ORIGINAL_IMAGE_URL,\n                otherwise ADDITIONAL_IMAGE_x - NOTE: for products with no ORIGINAL_IMAGE_URL, DETAIL_IMAGE_URL is consulted instead"),
            @Attribute(name = "productContentTypeIdList", type = "List", mode = "IN", optional = "true", description = "List of productContentTypeId of the ProductContent for the image - for main product image use ORIGINAL_IMAGE_URL,\n                otherwise ADDITIONAL_IMAGE_x - NOTE: for products with no ORIGINAL_IMAGE_URL, DETAIL_IMAGE_URL is consulted instead"),
            @Attribute(name = "contentId", type = "String", mode = "IN", optional = "true", description = "contentId of the ProductContent for the image [either contentId or productContentTypeId must be set]"),
            @Attribute(name = "contentIdList", type = "List", mode = "IN", optional = "true", description = "List of contentId of the ProductContent for the image [either contentId or productContentTypeId must be set]"),
            @Attribute(name = "sizeTypeList", type = "Collection", mode = "IN", optional = "true", description = "Optional list of size dimension names to restrict resizing to (e.g.: 320x240, small); unlisted are left unchanged"),
            @Attribute(name = "createSizeTypeContent", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true", description = "If true (default), ProductContent/DataResource records are added to products for size types not previously defined"),
            @Attribute(name = "recreateExisting", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If false (default), existing files for the size types are ignored and not regenerated; if true, all or give size types are always regenerated (slow)"),
            @Attribute(name = "nonFatal", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If true, runs in separate transaction and returns failure on image resize fail;\n                if false, runs in current transaction and returns error on on image resize fail"),
            @Attribute(name = "logDetail", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false"),
            @Attribute(name = "clearCaches", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false"),
            @Attribute(name = "imageOrigUrlMap", type = "Map", mode = "IN", optional = "true", description = "Maps productContentTypeId to original image URLs, intended for initial call; only needed if not already present on the product"),
            @Attribute(name = "imageOrigUrl", type = "String", mode = "IN", optional = "true", description = "imageOrigUrl for single productContentTypeId, same as imageOrigUrlMap[productContentTypeId]"),
            @Attribute(name = "copyOrig", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "Whether to make a copy of passed origImageUrl (if passed)"),
            @Attribute(name = "successCount", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "skipCount", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "errorCount", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "failCount", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "variantSuccessCount", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "variantFailCount", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "reason", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface ProductImageAutoRescale {}

    /**
     * Automatically rescales one or more product images for multiple or all products (does NOT consult parent/virtual products)
     */
    @Service(
        name = "productImageAutoRescaleProducts",
        engine = "java",
        location = "com.ilscipio.scipio.product.image.ProductImageServices",
        invoke = "productImageAutoRescaleProducts",
        description = "Automatically rescales one or more product images for multiple or all products (does NOT consult parent/virtual products)",
        transactionTimeout = "144000",
        attributes = {
            @Attribute(name = "products", type = "Object", mode = "IN", optional = "true", description = "List of product values or IDs or EntityListIterator"),
            @Attribute(name = "productIdList", type = "List", mode = "IN", optional = "true", description = "List of product IDs (alias for \"products\", same functionality, for easier use with runService UI)"),
            @Attribute(name = "allProducts", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If true, queries all products in the system instead of passing by list"),
            @Attribute(name = "allImages", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true", description = "(For each image) If true, automatically tries to apply to the known associated image types"),
            @Attribute(name = "productContentTypeId", type = "String", mode = "IN", optional = "true", description = "(For each image) productContentTypeId of the ProductContent for the image - for main product image use ORIGINAL_IMAGE_URL,\n                otherwise ADDITIONAL_IMAGE_x - NOTE: for products with no ORIGINAL_IMAGE_URL, DETAIL_IMAGE_URL is consulted instead"),
            @Attribute(name = "productContentTypeIdList", type = "List", mode = "IN", optional = "true", description = "(For each image) List of productContentTypeId of the ProductContent for the image - for main product image use ORIGINAL_IMAGE_URL,\n                otherwise ADDITIONAL_IMAGE_x - NOTE: for products with no ORIGINAL_IMAGE_URL, DETAIL_IMAGE_URL is consulted instead"),
            @Attribute(name = "sizeTypeList", type = "Collection", mode = "IN", optional = "true", description = "(For each image) Optional list of size dimension names to restrict resizing to (e.g.: 320x240, small); unlisted are left unchanged"),
            @Attribute(name = "createSizeTypeContent", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true", description = "If true (default), ProductContent/DataResource records are added to products for size types not previously defined"),
            @Attribute(name = "recreateExisting", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If false (default), existing files for the size types are ignored and not regenerated; if true, all or give size types are always regenerated (slow)"),
            @Attribute(name = "maxProducts", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "maxErrorCount", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "sepProductTrans", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true", description = "NOTE: For safety this is left true by default, but in addition each image may also get a separate transaction through nonFatal (TODO: clarify)"),
            @Attribute(name = "allCond", type = "org.ofbiz.entity.condition.EntityCondition", mode = "IN", optional = "true"),
            @Attribute(name = "allOrderBy", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "allResumeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "logBatch", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "logDetail", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false"),
            @Attribute(name = "successCount", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "skipCount", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "errorCount", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "failCount", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "variantSuccessCount", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "variantFailCount", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "failProductIdList", type = "List", mode = "OUT", optional = "true")
        }
    )
    public interface ProductImageAutoRescaleProducts {}

    /**
     * Automatically rescales one or more product images for multiple or all products (does NOT consult parent/virtual products)
     */
    @Service(
        name = "productImageAutoRescaleAll",
        engine = "java",
        location = "com.ilscipio.scipio.product.image.ProductImageServices",
        invoke = "productImageAutoRescaleAll",
        description = "Automatically rescales one or more product images for multiple or all products (does NOT consult parent/virtual products)",
        transactionTimeout = "172800",
        semaphore = "fail",
        implemented = {@Implements(service = "productImageAutoRescaleProducts")},
        overrideAttributes = {
            @OverrideAttribute(name = "allProducts", defaultValue = "true"),
            @OverrideAttribute(name = "allOrderBy", defaultValue = "[productId]"),
            @OverrideAttribute(name = "logBatch", defaultValue = "100"),
            @OverrideAttribute(name = "logDetail", defaultValue = "true")
        }
    )
    public interface ProductImageAutoRescaleAll {}

    /**
     * Aborts productImageAutoRescaleAll if possible
     */
    @Service(
        name = "abortProductImageAutoRescaleAll",
        engine = "java",
        location = "com.ilscipio.scipio.product.image.ProductImageServices",
        invoke = "abortProductImageAutoRescaleAll",
        description = "Aborts productImageAutoRescaleAll if possible",
        useTransaction = "false"
    )
    public interface AbortProductImageAutoRescaleAll {}

    /**
     * Clear ProductImageVariants caches (SCIPIO)
     */
    @Service(
        name = "productImageVariantsDistributedClearCaches",
        engine = "jms",
        location = "serviceMessenger",
        invoke = "productImageVariantsClearCaches",
        description = "Clear ProductImageVariants caches (SCIPIO)",
        auth = "true",
        useTransaction = "false",
        log = "quiet",
        logEca = "quiet",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productContentTypeId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface ProductImageVariantsDistributedClearCaches {}

    /**
     * Clear ProductImageVariants caches (SCIPIO)
     */
    @Service(
        name = "productImageVariantsClearCaches",
        engine = "java",
        location = "com.ilscipio.scipio.product.image.ProductImageVariants",
        invoke = "clearCaches",
        description = "Clear ProductImageVariants caches (SCIPIO)",
        auth = "true",
        export = "true",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productContentTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "distribute", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false")
        }
    )
    public interface ProductImageVariantsClearCaches {}

    /**
     * Migrates IMAGE_URL ProductContentType data for 2021-02 enhancements (SCIPIO)
     */
    @Service(
        name = "productImageMigrateImageUrlProductContentTypeData",
        engine = "java",
        location = "com.ilscipio.scipio.product.image.ProductImageServices",
        invoke = "productImageMigrateImageUrlProductContentTypeData",
        description = "Migrates IMAGE_URL ProductContentType data for 2021-02 enhancements (SCIPIO)",
        auth = "true",
        attributes = {
            @Attribute(name = "force", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If false, only runs if the ORIGINAL_IMAGE_URL record looks incomplete"),
            @Attribute(name = "forceAll", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If true, recreates all - implies force"),
            @Attribute(name = "preview", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true", description = "If true, aborts transaction at end")
        }
    )
    public interface ProductImageMigrateImageUrlProductContentTypeData {}

    /**
     * Run a ProductImageOpRequest from ECA (SCIPIO)
     */
    @Service(
        name = "productImageOpRequest",
        engine = "java",
        location = "com.ilscipio.scipio.product.image.ProductImageServices",
        invoke = "productImageOpRequest",
        description = "Run a ProductImageOpRequest from ECA (SCIPIO)",
        transactionTimeout = "144000",
        attributes = {
            @Attribute(name = "opReq", type = "GenericValue", mode = "IN", description = "ProductImageOpRequest instance"),
            @Attribute(name = "deleteReq", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If false, only runs if the ORIGINAL_IMAGE_URL record looks incomplete")
        }
    )
    public interface ProductImageOpRequest {}

    /**
     * Scales a product image file according to size types in product config ImageProperties.xml and using filename formats from catalog.properties
     */
    @Service(
        name = "categoryImageFileScaleInAllSize",
        engine = "java",
        location = "com.ilscipio.scipio.category.image.CategoryImageServices",
        invoke = "categoryImageFileScaleInAllSize",
        description = "Scales a product image file according to size types in product config ImageProperties.xml and using filename formats from catalog.properties",
        implemented = {@Implements(service = "imageFileScaleCommon")},
        attributes = {
            @Attribute(name = "productCategoryId", type = "String", mode = "IN", description = "productCategoryId"),
            @Attribute(name = "imageViewType", type = "com.ilscipio.scipio.category.image.CategoryImageViewType", mode = "IN", optional = "true", description = "Full instance alternative to viewType/viewNumber parameters (NOTE: imageViewType should be \"original\" viewSize only)"),
            @Attribute(name = "viewType", type = "String", mode = "IN", optional = "true", description = "main|additional"),
            @Attribute(name = "viewNumber", type = "Object", mode = "IN", optional = "true", description = "For additional (String or Integer), a number between 1-4 for stock records; 0 for main viewType"),
            @Attribute(name = "categorySizeTypeList", type = "List", mode = "OUT", optional = "true", description = "The list of typical product size types: [small, medium, large, detail]")
        }
    )
    public interface CategoryImageFileScaleInAllSize {}

    /**
     * Automatically rescales one or more category images for a category
     */
    @Service(
        name = "categoryImageAutoRescale",
        engine = "java",
        location = "com.ilscipio.scipio.category.image.CategoryImageServices",
        invoke = "categoryImageAutoRescale",
        description = "Automatically rescales one or more category images for a category",
        transactionTimeout = "1800",
        attributes = {
            @Attribute(name = "productCategoryId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productCategory", type = "GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "allImages", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If true, automatically tries to apply to all the known associated images or all prodCatContentTypeId"),
            @Attribute(name = "prodCatContentTypeId", type = "String", mode = "IN", optional = "true", description = "productContentTypeId of the ProductContent for the image - for main product image use ORIGINAL_IMAGE_URL,\n                otherwise ADDITIONAL_IMAGE_x - NOTE: for categories with no ORIGINAL_IMAGE_URL, DETAIL_IMAGE_URL is consulted instead"),
            @Attribute(name = "prodCatContentTypeIdList", type = "List", mode = "IN", optional = "true", description = "List of prodCatContentTypeId of the ProductCategoryContent for the image - for main product image use ORIGINAL_IMAGE_URL,\n                otherwise ADDITIONAL_IMAGE_x - NOTE: for categories with no ORIGINAL_IMAGE_URL, DETAIL_IMAGE_URL is consulted instead"),
            @Attribute(name = "contentId", type = "String", mode = "IN", optional = "true", description = "contentId of the ProductCategoryContent for the image [either contentId or prodCatContentTypeId must be set]"),
            @Attribute(name = "contentIdList", type = "List", mode = "IN", optional = "true", description = "List of contentId of the ProductCategoryContent for the image [either contentId or prodCatContentTypeId must be set]"),
            @Attribute(name = "sizeTypeList", type = "Collection", mode = "IN", optional = "true", description = "Optional list of size dimension names to restrict resizing to (e.g.: 320x240, small); unlisted are left unchanged"),
            @Attribute(name = "createSizeTypeContent", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true", description = "If true (default), ProductCategoryContent/DataResource records are added to categories for size types not previously defined"),
            @Attribute(name = "recreateExisting", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If false (default), existing files for the size types are ignored and not regenerated; if true, all or give size types are always regenerated (slow)"),
            @Attribute(name = "nonFatal", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If true, runs in separate transaction and returns failure on image resize fail;\n                if false, runs in current transaction and returns error on on image resize fail"),
            @Attribute(name = "logDetail", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false"),
            @Attribute(name = "clearCaches", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false"),
            @Attribute(name = "imageOrigUrlMap", type = "Map", mode = "IN", optional = "true", description = "Maps prodCatContentTypeId to original image URLs, intended for initial call; only needed if not already present on the category"),
            @Attribute(name = "imageOrigUrl", type = "String", mode = "IN", optional = "true", description = "imageOrigUrl for single prodCatContentTypeId, same as imageOrigUrlMap[prodCatContentTypeId]"),
            @Attribute(name = "copyOrig", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "Whether to make a copy of passed origImageUrl (if passed)"),
            @Attribute(name = "successCount", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "skipCount", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "errorCount", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "failCount", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "variantSuccessCount", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "variantFailCount", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "reason", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface CategoryImageAutoRescale {}

    /**
     * Automatically rescales one or more category images for multiple or all categories
     */
    @Service(
        name = "categoryImageAutoRescaleCategories",
        engine = "java",
        location = "com.ilscipio.scipio.category.image.CategoryImageServices",
        invoke = "categoryImageAutoRescaleCategories",
        description = "Automatically rescales one or more category images for multiple or all categories",
        transactionTimeout = "144000",
        attributes = {
            @Attribute(name = "productCategories", type = "Object", mode = "IN", optional = "true", description = "List of product categories values or IDs or EntityListIterator"),
            @Attribute(name = "productCategoryIdList", type = "List", mode = "IN", optional = "true", description = "List of category product IDs (alias for \"categories\", same functionality, for easier use with runService UI)"),
            @Attribute(name = "allProductCategories", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If true, queries all categories in the system instead of passing by list"),
            @Attribute(name = "allImages", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true", description = "(For each image) If true, automatically tries to apply to the known associated image types"),
            @Attribute(name = "prodCatContentTypeIdList", type = "String", mode = "IN", optional = "true", description = "(For each image) prodCatContentTypeIdList of the ProductCategoryContent for the image - for main product image use ORIGINAL_IMAGE_URL,\n                otherwise ADDITIONAL_IMAGE_x - NOTE: for products with no ORIGINAL_IMAGE_URL, DETAIL_IMAGE_URL is consulted instead"),
            @Attribute(name = "prodCatContentTypeIdList", type = "List", mode = "IN", optional = "true", description = "(For each image) List of prodCatContentTypeId of the ProductCategoryContent for the image - for main product image use ORIGINAL_IMAGE_URL,\n                otherwise ADDITIONAL_IMAGE_x - NOTE: for products with no ORIGINAL_IMAGE_URL, DETAIL_IMAGE_URL is consulted instead"),
            @Attribute(name = "sizeTypeList", type = "Collection", mode = "IN", optional = "true", description = "(For each image) Optional list of size dimension names to restrict resizing to (e.g.: 320x240, small); unlisted are left unchanged"),
            @Attribute(name = "createSizeTypeContent", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true", description = "If true (default), ProductCategoryContent/DataResource records are added to products for size types not previously defined"),
            @Attribute(name = "recreateExisting", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If false (default), existing files for the size types are ignored and not regenerated; if true, all or give size types are always regenerated (slow)"),
            @Attribute(name = "maxProductCategories", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "maxErrorCount", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "sepProductCategoryTrans", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true", description = "NOTE: For safety this is left true by default, but in addition each image may also get a separate transaction through nonFatal (TODO: clarify)"),
            @Attribute(name = "allCond", type = "org.ofbiz.entity.condition.EntityCondition", mode = "IN", optional = "true"),
            @Attribute(name = "allOrderBy", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "allResumeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "logBatch", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "logDetail", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false"),
            @Attribute(name = "successCount", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "skipCount", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "errorCount", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "failCount", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "failCategoryIdList", type = "List", mode = "OUT", optional = "true")
        }
    )
    public interface CategoryImageAutoRescaleCategories {}

    /**
     * Automatically rescales one or more category images for multiple or all categories
     */
    @Service(
        name = "categoryImageAutoRescaleAll",
        engine = "java",
        location = "com.ilscipio.scipio.category.image.CategoryImageServices",
        invoke = "categoryImageAutoRescaleAll",
        description = "Automatically rescales one or more category images for multiple or all categories",
        transactionTimeout = "172800",
        semaphore = "fail",
        implemented = {@Implements(service = "categoryImageAutoRescaleCategories")},
        overrideAttributes = {
            @OverrideAttribute(name = "allCategories", defaultValue = "true"),
            @OverrideAttribute(name = "allOrderBy", defaultValue = "[productCategoryId]"),
            @OverrideAttribute(name = "logBatch", defaultValue = "100"),
            @OverrideAttribute(name = "logDetail", defaultValue = "true")
        }
    )
    public interface CategoryImageAutoRescaleAll {}

    /**
     * Aborts categoryImageAutoRescaleAll if possible
     */
    @Service(
        name = "abortCategoryImageAutoRescaleAll",
        engine = "java",
        location = "com.ilscipio.scipio.category.image.CategoryImageServices",
        invoke = "abortCategoryImageAutoRescaleAll",
        description = "Aborts categoryImageAutoRescaleAll if possible",
        useTransaction = "false"
    )
    public interface AbortCategoryImageAutoRescaleAll {}

    /**
     * Migrates CATEGORY_IMAGE_URL ProdCatContentType data for 2022-09 enhancements (SCIPIO)
     */
    @Service(
        name = "categoryImageMigrateImageUrlCategoryContentTypeData",
        engine = "java",
        location = "com.ilscipio.scipio.category.image.CategoryImageServices",
        invoke = "categoryImageMigrateImageUrlCategoryContentTypeData",
        description = "Migrates CATEGORY_IMAGE_URL ProdCatContentType data for 2022-09 enhancements (SCIPIO)",
        auth = "true",
        attributes = {
            @Attribute(name = "force", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If false, only runs if the ORIGINAL_IMAGE_URL record looks incomplete"),
            @Attribute(name = "forceAll", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If true, recreates all - implies force"),
            @Attribute(name = "preview", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true", description = "If true, aborts transaction at end")
        }
    )
    public interface CategoryImageMigrateImageUrlCategoryContentTypeData {}

    /**
     * Run a CategoryImageOpRequest from ECA (SCIPIO)
     */
    @Service(
        name = "categoryImageOpRequest",
        engine = "java",
        location = "com.ilscipio.scipio.category.image.CategoryImageServices",
        invoke = "categoryImageOpRequest",
        description = "Run a CategoryImageOpRequest from ECA (SCIPIO)",
        transactionTimeout = "144000",
        attributes = {
            @Attribute(name = "opReq", type = "GenericValue", mode = "IN", description = "CategoryImageOpRequest instance"),
            @Attribute(name = "deleteReq", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If false, only runs if the CATEGORY_IMAGE_URL record looks incomplete")
        }
    )
    public interface CategoryImageOpRequest {}

    /**
     * Clear CategoryImageVariants caches (SCIPIO)
     */
    @Service(
        name = "categoryImageVariantsDistributedClearCaches",
        engine = "jms",
        location = "serviceMessenger",
        invoke = "categoryImageVariantsClearCaches",
        description = "Clear CategoryImageVariants caches (SCIPIO)",
        auth = "true",
        useTransaction = "false",
        log = "quiet",
        logEca = "quiet",
        attributes = {
            @Attribute(name = "productCategoryId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "prodCatContentTypeId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface CategoryImageVariantsDistributedClearCaches {}

    /**
     * Clear CategoryImageVariants caches (SCIPIO)
     */
    @Service(
        name = "categoryImageVariantsClearCaches",
        engine = "java",
        location = "com.ilscipio.scipio.category.image.CategoryImageVariants",
        invoke = "clearCaches",
        description = "Clear CategoryImageVariants caches (SCIPIO)",
        auth = "true",
        export = "true",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "productCategoryId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "prodCatContentTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "distribute", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false")
        }
    )
    public interface CategoryImageVariantsClearCaches {}

}

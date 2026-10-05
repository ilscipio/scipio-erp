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
package com.ilscipio.scipio.manufacturing.service;

import com.ilscipio.scipio.service.def.*;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class BomServices {

    /**
     * Add Product to Product Association
     */
    @Service(
        name = "createBOMAssoc",
        engine = "java",
        location = "com.ilscipio.scipio.manufacturing.event.BomSimpleMethods",
        invoke = "createBOMAssoc",
        description = "Add Product to Product Association",
        defaultEntityName = "ProductAssoc",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "errorMessage", type = "String", mode = "OUT", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "true")
        }
    )
    public interface CreateBOMAssoc {}

    /**
     * Copy BOM associations from one product to another
     */
    @Service(
        name = "copyBOMAssocs",
        engine = "java",
        location = "com.ilscipio.scipio.manufacturing.event.BomSimpleMethods",
        invoke = "copyBOMAssocs",
        description = "Copy BOM associations from one product to another",
        defaultEntityName = "ProductAssoc",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "copyToProductId", type = "String", mode = "IN")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "true"),
            @OverrideAttribute(name = "productIdTo", optional = "true")
        }
    )
    public interface CopyBOMAssocs {}

    /**
     * Update a Product Manufacturing Rule
     */
    @Service(
        name = "updateProductManufacturingRule",
        engine = "java",
        location = "com.ilscipio.scipio.manufacturing.event.BomSimpleMethods",
        invoke = "updateProductManufacturingRule",
        description = "Update a Product Manufacturing Rule",
        defaultEntityName = "ProductManufacturingRule",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true", excludeFields = {"ruleSeqId", "description"})
        },
        overrideAttributes = {
            @OverrideAttribute(name = "productIdIn", optional = "false")
        }
    )
    public interface UpdateProductManufacturingRule {}

    /**
     * Create a Product Manufacturing Rule
     */
    @Service(
        name = "addProductManufacturingRule",
        engine = "java",
        location = "com.ilscipio.scipio.manufacturing.event.BomSimpleMethods",
        invoke = "addProductManufacturingRule",
        description = "Create a Product Manufacturing Rule",
        defaultEntityName = "ProductManufacturingRule",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true", excludeFields = {"ruleSeqId", "description"})
        },
        overrideAttributes = {
            @OverrideAttribute(name = "productIdIn", optional = "false")
        }
    )
    public interface AddProductManufacturingRule {}

    /**
     * Remove a Product Manufacturing Rule
     */
    @Service(
        name = "deleteProductManufacturingRule",
        engine = "java",
        location = "com.ilscipio.scipio.manufacturing.event.BomSimpleMethods",
        invoke = "deleteProductManufacturingRule",
        description = "Remove a Product Manufacturing Rule",
        attributes = {
            @Attribute(name = "ruleId", type = "String", mode = "IN", formLabel = "${uiLabelMap.ManufacturingRuleId}")
        }
    )
    public interface DeleteProductManufacturingRule {}

    /**
     * Returns the max product's depth in the bill of materials
     */
    @Service(
        name = "getMaxDepth",
        engine = "java",
        location = "org.ofbiz.manufacturing.bom.BOMServices",
        invoke = "getMaxDepth",
        description = "Returns the max product's depth in the bill of materials",
        defaultEntityName = "ProductAssoc",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "fromDate", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "bomType", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "depth", type = "Long", mode = "OUT")
        }
    )
    public interface GetMaxDepth {}

    /**
     * Updates the low level code of the product in the Product entity
     */
    @Service(
        name = "updateLowLevelCode",
        engine = "java",
        location = "org.ofbiz.manufacturing.bom.BOMServices",
        invoke = "updateLowLevelCode",
        description = "Updates the low level code of the product in the Product entity",
        attributes = {
            @Attribute(name = "productIdTo", type = "String", mode = "IN"),
            @Attribute(name = "alsoComponents", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "alsoVariants", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "lowLevelCode", type = "Long", mode = "OUT")
        }
    )
    public interface UpdateLowLevelCode {}

    /**
     * Updates the low level code of all the products in the Product entity
     */
    @Service(
        name = "initLowLevelCode",
        engine = "java",
        location = "org.ofbiz.manufacturing.bom.BOMServices",
        invoke = "initLowLevelCode",
        description = "Updates the low level code of all the products in the Product entity",
        transactionTimeout = "7200"
    )
    public interface InitLowLevelCode {}

    /**
     * Returns the ProductAssoc generic value for a duplicate productIdTo ancestor if present, null otherwise. Useful to avoid loops when adding new assocs to a bill of materials.
     */
    @Service(
        name = "searchDuplicatedAncestor",
        engine = "java",
        location = "org.ofbiz.manufacturing.bom.BOMServices",
        invoke = "searchDuplicatedAncestor",
        description = "Returns the ProductAssoc generic value for a duplicate productIdTo ancestor if present, null otherwise. Useful to avoid loops when adding new assocs to a bill of materials.",
        defaultEntityName = "ProductAssoc",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "productIdTo", type = "String", mode = "IN"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "productAssocTypeId", type = "String", mode = "IN"),
            @Attribute(name = "duplicatedProductAssoc", type = "org.ofbiz.entity.GenericValue", mode = "OUT", optional = "true")
        }
    )
    public interface SearchDuplicatedAncestor {}

    /**
     * Returns a BOMTree (an object that represents a configured bill of material tree in memory). Useful for tree traversal (breakdown, explosion, implosion).
     */
    @Service(
        name = "getBOMTree",
        engine = "java",
        location = "org.ofbiz.manufacturing.bom.BOMServices",
        invoke = "getBOMTree",
        description = "Returns a BOMTree (an object that represents a configured bill of material tree in memory). Useful for tree traversal (breakdown, explosion, implosion).",
        defaultEntityName = "ProductAssoc",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "type", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "fromDate", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "bomType", type = "String", mode = "IN"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "amount", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "tree", type = "org.ofbiz.manufacturing.bom.BOMTree", mode = "OUT", optional = "true")
        }
    )
    public interface GetBOMTree {}

    /**
     * Returns the product's routing id and the components of a given product (if necessary, running the configurator).
     */
    @Service(
        name = "getManufacturingComponents",
        engine = "java",
        location = "org.ofbiz.manufacturing.bom.BOMServices",
        invoke = "getManufacturingComponents",
        description = "Returns the product's routing id and the components of a given product (if necessary, running the configurator).",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "amount", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "fromDate", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "excludeWIPs", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "workEffortId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "components", type = "java.util.List", mode = "OUT", optional = "true"),
            @Attribute(name = "componentsMap", type = "java.util.List", mode = "OUT", optional = "true")
        }
    )
    public interface GetManufacturingComponents {}

    /**
     * Returns the components (that needs to be packaged) of a given product (if necessary, running the configurator).
     */
    @Service(
        name = "getProductsInPackages",
        engine = "java",
        location = "org.ofbiz.manufacturing.bom.BOMServices",
        invoke = "getProductsInPackages",
        description = "Returns the components (that needs to be packaged) of a given product (if necessary, running the configurator).",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "fromDate", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productsInPackages", type = "java.util.List", mode = "OUT")
        }
    )
    public interface GetProductsInPackages {}

    /**
     * Explodes a product id and returns all the components that are not manufactured on customer order: these components will be taken from warehouse.
     */
    @Service(
        name = "getNotAssembledComponents",
        engine = "java",
        location = "org.ofbiz.manufacturing.bom.BOMServices",
        invoke = "getNotAssembledComponents",
        description = "Explodes a product id and returns all the components that are not manufactured on customer order: these components will be taken from warehouse.",
        auth = "true",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "amount", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "fromDate", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "notAssembledComponents", type = "java.util.List", mode = "OUT")
        }
    )
    public interface GetNotAssembledComponents {}

}

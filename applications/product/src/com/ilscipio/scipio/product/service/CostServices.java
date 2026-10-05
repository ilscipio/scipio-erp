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
public class CostServices {

    /**
     * Create a CostComponent
     */
    @Service(
        name = "createCostComponent",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a CostComponent",
        defaultEntityName = "CostComponent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "OUT", include = "pk")
        }
    )
    public interface CreateCostComponent {}

    /**
     * Update a CostComponent
     */
    @Service(
        name = "updateCostComponent",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a CostComponent",
        defaultEntityName = "CostComponent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateCostComponent {}

    /**
     * Delete a CostComponent
     */
    @Service(
        name = "deleteCostComponent",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a CostComponent",
        defaultEntityName = "CostComponent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteCostComponent {}

    /**
     * Create a CostComponent and cancel the existing ones
     */
    @Service(
        name = "recreateCostComponent",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/cost/CostServices.xml",
        invoke = "recreateCostComponent",
        description = "Create a CostComponent and cancel the existing ones",
        defaultEntityName = "CostComponent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "OUT", include = "pk")
        }
    )
    public interface RecreateCostComponent {}

    /**
     * Cancels CostComponent
     */
    @Service(
        name = "cancelCostComponents",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/cost/CostServices.xml",
        invoke = "cancelCostComponents",
        description = "Cancels CostComponent",
        auth = "true",
        attributes = {
            @Attribute(name = "costComponentId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "costUomId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "costComponentTypeId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface CancelCostComponents {}

    /**
     * Create a ProductCostComponentCalc
     */
    @Service(
        name = "createProductCostComponentCalc",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ProductCostComponentCalc",
        defaultEntityName = "ProductCostComponentCalc",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateProductCostComponentCalc {}

    /**
     * Update a ProductCostComponentCalc
     */
    @Service(
        name = "updateProductCostComponentCalc",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ProductCostComponentCalc",
        defaultEntityName = "ProductCostComponentCalc",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateProductCostComponentCalc {}

    /**
     * Delete a Example
     */
    @Service(
        name = "deleteProductCostComponentCalc",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a Example",
        defaultEntityName = "ProductCostComponentCalc",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteProductCostComponentCalc {}

    /**
     * Gets the product's costs from CostComponent entries
     */
    @Service(
        name = "getProductCost",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/cost/CostServices.xml",
        invoke = "getProductCost",
        description = "Gets the product's costs from CostComponent entries",
        auth = "true",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "currencyUomId", type = "String", mode = "IN"),
            @Attribute(name = "costComponentTypePrefix", type = "String", mode = "IN"),
            @Attribute(name = "productCost", type = "BigDecimal", mode = "OUT")
        }
    )
    public interface GetProductCost {}

    /**
     * Gets the production run task's costs
     */
    @Service(
        name = "getTaskCost",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/cost/CostServices.xml",
        invoke = "getTaskCost",
        description = "Gets the production run task's costs",
        auth = "true",
        attributes = {
            @Attribute(name = "workEffortId", type = "String", mode = "IN"),
            @Attribute(name = "currencyUomId", type = "String", mode = "IN"),
            @Attribute(name = "productId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "routingId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "taskCost", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "costsByType", type = "Map", mode = "OUT", optional = "true")
        }
    )
    public interface GetTaskCost {}

    /**
     * Calculates the product's costs. If the product does not have cost component defined, will use the BOM to calculate the cost.
     */
    @Service(
        name = "calculateProductCosts",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/cost/CostServices.xml",
        invoke = "calculateProductCosts",
        description = "Calculates the product's costs. If the product does not have cost component defined, will use the BOM to calculate the cost.",
        auth = "true",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "currencyUomId", type = "String", mode = "IN"),
            @Attribute(name = "costComponentTypePrefix", type = "String", mode = "IN"),
            @Attribute(name = "totalCost", type = "BigDecimal", mode = "OUT")
        }
    )
    public interface CalculateProductCosts {}

    /**
     * Calculates estimated costs for all the products
     */
    @Service(
        name = "calculateAllProductsCosts",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/cost/CostServices.xml",
        invoke = "calculateAllProductsCosts",
        description = "Calculates estimated costs for all the products",
        auth = "true",
        transactionTimeout = "7200",
        attributes = {
            @Attribute(name = "currencyUomId", type = "String", mode = "IN"),
            @Attribute(name = "costComponentTypePrefix", type = "String", mode = "IN")
        }
    )
    public interface CalculateAllProductsCosts {}

    /**
     * Calculate inventory average cost for a product
     */
    @Service(
        name = "calculateProductAverageCost",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/cost/CostServices.xml",
        invoke = "calculateProductAverageCost",
        description = "Calculate inventory average cost for a product",
        auth = "true",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "facilityId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "ownerPartyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "totalQuantityOnHand", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "totalInventoryCost", type = "BigDecimal", mode = "OUT", optional = "true"),
            @Attribute(name = "productAverageCost", type = "BigDecimal", mode = "OUT", optional = "true"),
            @Attribute(name = "currencyUomId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface CalculateProductAverageCost {}

    /**
     * Interface to describe base parameters for Product Cost Calculation Services
     */
    @Service(
        name = "productCostCalcInterface",
        engine = "interface",
        description = "Interface to describe base parameters for Product Cost Calculation Services",
        attributes = {
            @Attribute(name = "productCostComponentCalc", type = "GenericValue", mode = "IN"),
            @Attribute(name = "costComponentCalc", type = "GenericValue", mode = "IN"),
            @Attribute(name = "currencyUomId", type = "String", mode = "IN"),
            @Attribute(name = "costComponentTypePrefix", type = "String", mode = "IN"),
            @Attribute(name = "baseCost", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "productCostAdjustment", type = "BigDecimal", mode = "OUT")
        }
    )
    public interface ProductCostCalcInterface {}

    /**
     * Formula that creates a cost component equal to a percentage of total product cost
     */
    @Service(
        name = "productCostPercentageFormula",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/cost/CostServices.xml",
        invoke = "productCostPercentageFormula",
        description = "Formula that creates a cost component equal to a percentage of total product cost",
        auth = "true",
        implemented = {@Implements(service = "productCostCalcInterface")}
    )
    public interface ProductCostPercentageFormula {}

    /**
     * Create a new Cost Component Attribute Record
     */
    @Service(
        name = "createCostComponentAttribute",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a new Cost Component Attribute Record",
        defaultEntityName = "CostComponentAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateCostComponentAttribute {}

    /**
     * Update a CostComponentAttribute
     */
    @Service(
        name = "updateCostComponentAttribute",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a CostComponentAttribute",
        defaultEntityName = "CostComponentAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateCostComponentAttribute {}

    /**
     * Delete an existing CostComponentAttribute Record
     */
    @Service(
        name = "deleteCostComponentAttribute",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete an existing CostComponentAttribute Record",
        defaultEntityName = "CostComponentAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteCostComponentAttribute {}

    /**
     * Create a new Cost Component Type Record
     */
    @Service(
        name = "createCostComponentType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a new Cost Component Type Record",
        defaultEntityName = "CostComponentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateCostComponentType {}

    /**
     * Update a Cost Component Type
     */
    @Service(
        name = "updateCostComponentType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Cost Component Type",
        defaultEntityName = "CostComponentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateCostComponentType {}

    /**
     * Delete an existing Cost Component Type Record
     */
    @Service(
        name = "deleteCostComponentType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete an existing Cost Component Type Record",
        defaultEntityName = "CostComponentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteCostComponentType {}

    /**
     * Create a new Cost Component Type Record
     */
    @Service(
        name = "createCostComponentTypeAttr",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a new Cost Component Type Record",
        defaultEntityName = "CostComponentTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateCostComponentTypeAttr {}

    /**
     * Update a Cost Component Type
     */
    @Service(
        name = "updateCostComponentTypeAttr",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Cost Component Type",
        defaultEntityName = "CostComponentTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateCostComponentTypeAttr {}

    /**
     * Delete an existing Cost Component Type Attr Record
     */
    @Service(
        name = "deleteCostComponentTypeAttr",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete an existing Cost Component Type Attr Record",
        defaultEntityName = "CostComponentTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteCostComponentTypeAttr {}

}

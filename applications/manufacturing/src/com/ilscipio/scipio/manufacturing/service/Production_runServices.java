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
public class Production_runServices {

    /**
     * Create a Production Run
     */
    @Service(
        name = "createProductionRun",
        engine = "java",
        location = "org.ofbiz.manufacturing.jobshopmgt.ProductionRunServices",
        invoke = "createProductionRun",
        description = "Create a Production Run",
        auth = "true",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "pRQuantity", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "startDate", type = "java.sql.Timestamp", mode = "IN"),
            @Attribute(name = "facilityId", type = "String", mode = "IN"),
            @Attribute(name = "routingId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "workEffortName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "description", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productionRunId", type = "String", mode = "OUT"),
            @Attribute(name = "estimatedCompletionDate", type = "java.sql.Timestamp", mode = "OUT", optional = "true")
        }
    )
    public interface CreateProductionRun {}

    /**
     *              Associate a party to the production run         
     */
    @Service(
        name = "createProductionRunPartyAssign",
        engine = "java",
        location = "com.ilscipio.scipio.manufacturing.event.ProductionRunSimpleServices",
        invoke = "createProductionRunPartyAssign",
        description = "\n            Associate a party to the production run\n        ",
        attributes = {
            @Attribute(name = "productionRunId", type = "String", mode = "INOUT"),
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "roleTypeId", type = "String", mode = "IN"),
            @Attribute(name = "workEffortId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fromDate", type = "java.sql.Timestamp", mode = "INOUT", optional = "true"),
            @Attribute(name = "thruDate", type = "Timestamp", mode = "IN", optional = "true")
        }
    )
    public interface CreateProductionRunPartyAssign {}

    /**
     *              Associate the production run to another production run         
     */
    @Service(
        name = "createProductionRunAssoc",
        engine = "java",
        location = "com.ilscipio.scipio.manufacturing.event.ProductionRunSimpleServices",
        invoke = "createProductionRunAssoc",
        description = "\n            Associate the production run to another production run\n        ",
        attributes = {
            @Attribute(name = "productionRunId", type = "String", mode = "IN"),
            @Attribute(name = "productionRunIdTo", type = "String", mode = "IN"),
            @Attribute(name = "workFlowSequenceTypeId", type = "String", mode = "IN")
        }
    )
    public interface CreateProductionRunAssoc {}

    /**
     * Explodes a product id and creates all the needed production runs.
     */
    @Service(
        name = "createProductionRunsForProductBom",
        engine = "java",
        location = "org.ofbiz.manufacturing.jobshopmgt.ProductionRunServices",
        invoke = "createProductionRunsForProductBom",
        description = "Explodes a product id and creates all the needed production runs.",
        auth = "true",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "startDate", type = "java.sql.Timestamp", mode = "IN"),
            @Attribute(name = "facilityId", type = "String", mode = "IN"),
            @Attribute(name = "routingId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "workEffortName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "description", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productionRuns", type = "java.util.List", mode = "OUT", optional = "true"),
            @Attribute(name = "productionRunId", type = "String", mode = "OUT")
        }
    )
    public interface CreateProductionRunsForProductBom {}

    /**
     * Explodes a product id and creates all the needed production runs; if an order id is also provided, it links the production runs to the sales order.
     */
    @Service(
        name = "createProductionRunsForOrder",
        engine = "java",
        location = "org.ofbiz.manufacturing.jobshopmgt.ProductionRunServices",
        invoke = "createProductionRunsForOrder",
        description = "Explodes a product id and creates all the needed production runs; if an order id is also provided, it links the production runs to the sales order.",
        auth = "true",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "orderItemSeqId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "shipGroupSeqId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "fromDate", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "shipmentId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productionRuns", type = "java.util.List", mode = "OUT")
        }
    )
    public interface CreateProductionRunsForOrder {}

    /**
     * Creates a production run from a requirement.
     */
    @Service(
        name = "createProductionRunFromRequirement",
        engine = "java",
        location = "org.ofbiz.manufacturing.jobshopmgt.ProductionRunServices",
        invoke = "createProductionRunFromRequirement",
        description = "Creates a production run from a requirement.",
        auth = "true",
        attributes = {
            @Attribute(name = "requirementId", type = "String", mode = "IN"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "productionRunId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface CreateProductionRunFromRequirement {}

    /**
     * Creates a production run from a product configuration.
     */
    @Service(
        name = "createProductionRunFromConfiguration",
        engine = "java",
        location = "org.ofbiz.manufacturing.jobshopmgt.ProductionRunServices",
        invoke = "createProductionRunFromConfiguration",
        description = "Creates a production run from a product configuration.",
        auth = "true",
        attributes = {
            @Attribute(name = "facilityId", type = "String", mode = "IN"),
            @Attribute(name = "configId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "config", type = "org.ofbiz.product.config.ProductConfigWrapper", mode = "IN", optional = "true"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "orderId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "orderItemSeqId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productionRunId", type = "String", mode = "OUT")
        }
    )
    public interface CreateProductionRunFromConfiguration {}

    /**
     * Creates a production run for a marketing package when the product is out of stock (ATP quantity less than zero.)                 Attempts to produce enough to bring total ATP quantity of the product back up to zero, but will only produce what is                 available based on the components required.
     */
    @Service(
        name = "createProductionRunForMktgPkg",
        engine = "java",
        location = "org.ofbiz.manufacturing.jobshopmgt.ProductionRunServices",
        invoke = "createProductionRunForMktgPkg",
        description = "Creates a production run for a marketing package when the product is out of stock (ATP quantity less than zero.)\n                Attempts to produce enough to bring total ATP quantity of the product back up to zero, but will only produce what is\n                available based on the components required.",
        auth = "true",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "orderItemSeqId", type = "String", mode = "IN"),
            @Attribute(name = "facilityId", type = "String", mode = "IN"),
            @Attribute(name = "productionRunId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface CreateProductionRunForMktgPkg {}

    /**
     * Update a Production Run
     */
    @Service(
        name = "updateProductionRun",
        engine = "java",
        location = "org.ofbiz.manufacturing.jobshopmgt.ProductionRunServices",
        invoke = "updateProductionRun",
        description = "Update a Production Run",
        auth = "true",
        attributes = {
            @Attribute(name = "productionRunId", type = "String", mode = "IN"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "estimatedStartDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "workEffortName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "description", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "facilityId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface UpdateProductionRun {}

    /**
     * Change the Production Run status
     */
    @Service(
        name = "changeProductionRunStatus",
        engine = "java",
        location = "org.ofbiz.manufacturing.jobshopmgt.ProductionRunServices",
        invoke = "changeProductionRunStatus",
        description = "Change the Production Run status",
        auth = "true",
        attributes = {
            @Attribute(name = "productionRunId", type = "String", mode = "IN"),
            @Attribute(name = "statusId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "newStatusId", type = "String", mode = "OUT")
        }
    )
    public interface ChangeProductionRunStatus {}

    /**
     * Change the Production Run Task status
     */
    @Service(
        name = "changeProductionRunTaskStatus",
        engine = "java",
        location = "org.ofbiz.manufacturing.jobshopmgt.ProductionRunServices",
        invoke = "changeProductionRunTaskStatus",
        description = "Change the Production Run Task status",
        auth = "true",
        attributes = {
            @Attribute(name = "productionRunId", type = "String", mode = "IN"),
            @Attribute(name = "workEffortId", type = "String", mode = "IN"),
            @Attribute(name = "statusId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "issueAllComponents", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "oldStatusId", type = "String", mode = "OUT"),
            @Attribute(name = "newStatusId", type = "String", mode = "OUT")
        }
    )
    public interface ChangeProductionRunTaskStatus {}

    /**
     * add a RoutingTask to an existing ProductionRun
     */
    @Service(
        name = "addProductionRunRoutingTask",
        engine = "java",
        location = "org.ofbiz.manufacturing.jobshopmgt.ProductionRunServices",
        invoke = "addProductionRunRoutingTask",
        description = "add a RoutingTask to an existing ProductionRun",
        auth = "true",
        attributes = {
            @Attribute(name = "productionRunId", type = "String", mode = "IN"),
            @Attribute(name = "routingTaskId", type = "String", mode = "INOUT"),
            @Attribute(name = "priority", type = "Long", mode = "IN"),
            @Attribute(name = "estimatedSetupMillis", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "estimatedMilliSeconds", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "workEffortName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "description", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "estimatedStartDate", type = "Timestamp", mode = "INOUT", optional = "true"),
            @Attribute(name = "estimatedCompletionDate", type = "Timestamp", mode = "INOUT", optional = "true")
        }
    )
    public interface AddProductionRunRoutingTask {}

    /**
     * check if field for routingTask update are correct and if needed  recalculated data and update Production Run
     */
    @Service(
        name = "checkUpdatePrunRoutingTask",
        engine = "java",
        location = "org.ofbiz.manufacturing.jobshopmgt.ProductionRunServices",
        invoke = "checkUpdatePrunRoutingTask",
        description = "check if field for routingTask update are correct and if needed  recalculated data and update Production Run",
        auth = "true",
        attributes = {
            @Attribute(name = "productionRunId", type = "String", mode = "IN"),
            @Attribute(name = "routingTaskId", type = "String", mode = "IN"),
            @Attribute(name = "priority", type = "Long", mode = "IN"),
            @Attribute(name = "estimatedStartDate", type = "Timestamp", mode = "IN"),
            @Attribute(name = "estimatedSetupMillis", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "estimatedMilliSeconds", type = "BigDecimal", mode = "IN")
        }
    )
    public interface CheckUpdatePrunRoutingTask {}

    /**
     * add a Product Component to an existing ProductionRun
     */
    @Service(
        name = "addProductionRunComponent",
        engine = "java",
        location = "org.ofbiz.manufacturing.jobshopmgt.ProductionRunServices",
        invoke = "addProductionRunComponent",
        description = "add a Product Component to an existing ProductionRun",
        auth = "true",
        attributes = {
            @Attribute(name = "productionRunId", type = "String", mode = "IN"),
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "estimatedQuantity", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "workEffortId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface AddProductionRunComponent {}

    /**
     * update a Product Component to an existing ProductionRun
     */
    @Service(
        name = "updateProductionRunComponent",
        engine = "java",
        location = "org.ofbiz.manufacturing.jobshopmgt.ProductionRunServices",
        invoke = "updateProductionRunComponent",
        description = "update a Product Component to an existing ProductionRun",
        auth = "true",
        attributes = {
            @Attribute(name = "productionRunId", type = "String", mode = "IN"),
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "workEffortId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "estimatedQuantity", type = "BigDecimal", mode = "IN", optional = "true")
        }
    )
    public interface UpdateProductionRunComponent {}

    /**
     * replace a Product Component on an existing ProductionRun task with a different product
     */
    @Service(
        name = "replaceProductionRunComponent",
        engine = "java",
        location = "com.ilscipio.scipio.manufacturing.event.ProductionRunSimpleServices",
        invoke = "replaceProductionRunComponent",
        description = "replace a Product Component on an existing ProductionRun task with a different product",
        auth = "true",
        attributes = {
            @Attribute(name = "productionRunId", type = "String", mode = "IN"),
            @Attribute(name = "workEffortId", type = "String", mode = "IN"),
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "newProductId", type = "String", mode = "IN"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN", optional = "true")
        }
    )
    public interface ReplaceProductionRunComponent {}

    /**
     *              Issues the Inventory for a Production Run Task.             Note that this skips the normal inventory reservation process.         
     */
    @Service(
        name = "issueProductionRunTask",
        engine = "java",
        location = "com.ilscipio.scipio.manufacturing.event.ProductionRunSimpleServices",
        invoke = "issueProductionRunTask",
        description = "\n            Issues the Inventory for a Production Run Task.\n            Note that this skips the normal inventory reservation process.\n        ",
        attributes = {
            @Attribute(name = "workEffortId", type = "String", mode = "IN"),
            @Attribute(name = "reserveOrderEnumId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "failIfItemsAreNotAvailable", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "failIfItemsAreNotOnHand", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface IssueProductionRunTask {}

    /**
     *              Issues the Inventory for a Production Run Task Component. For more info see the issueProductionRunTask service.             If fromDate is passed, then the WorkEffortGoodStandard record with pk composed of (workEffortId|productId|fromDate)             with type PRUNT_PROD_NEEDED is retrieved and used to get the quantity; its status is also updated to COMPLETED after             the issuance is done.             If locationSeqIds are provided, then the items are only issued from the inventory items associated to the locations.             If failIfItemsAreNotAvailable is set to "Y" (the default is "Y") then the service fails if there is not enough inventory available:             no reservation will be stolen.             If failIfItemsAreNotOnHand is set to "Y" (the default is "Y") then the service fails if there is not enough inventory:             no items with negative qoh will be created.             If lotId is filled, failIfItemsAreNotAvailable is set to automatically set to "Y".         
     */
    @Service(
        name = "issueProductionRunTaskComponent",
        engine = "java",
        location = "com.ilscipio.scipio.manufacturing.event.ProductionRunSimpleServices",
        invoke = "issueProductionRunTaskComponent",
        description = "\n            Issues the Inventory for a Production Run Task Component. For more info see the issueProductionRunTask service.\n            If fromDate is passed, then the WorkEffortGoodStandard record with pk composed of (workEffortId|productId|fromDate)\n            with type PRUNT_PROD_NEEDED is retrieved and used to get the quantity; its status is also updated to COMPLETED after\n            the issuance is done.\n            If locationSeqIds are provided, then the items are only issued from the inventory items associated to the locations.\n            If failIfItemsAreNotAvailable is set to \"Y\" (the default is \"Y\") then the service fails if there is not enough inventory available:\n            no reservation will be stolen.\n            If failIfItemsAreNotOnHand is set to \"Y\" (the default is \"Y\") then the service fails if there is not enough inventory:\n            no items with negative qoh will be created.\n            If lotId is filled, failIfItemsAreNotAvailable is set to automatically set to \"Y\".\n        ",
        attributes = {
            @Attribute(name = "workEffortId", type = "String", mode = "IN"),
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "failIfItemsAreNotAvailable", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "failIfItemsAreNotOnHand", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "reserveOrderEnumId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "lotId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "locationSeqId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "secondaryLocationSeqId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "reasonEnumId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "description", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface IssueProductionRunTaskComponent {}

    /**
     *              Issue one InventoryItem (or part of it) to a WorkEffort.             Note that this skips the normal inventory reservation process.         
     */
    @Service(
        name = "issueInventoryItemToWorkEffort",
        engine = "java",
        location = "com.ilscipio.scipio.manufacturing.event.ProductionRunSimpleServices",
        invoke = "issueInventoryItemToWorkEffort",
        description = "\n            Issue one InventoryItem (or part of it) to a WorkEffort.\n            Note that this skips the normal inventory reservation process.\n        ",
        attributes = {
            @Attribute(name = "workEffortId", type = "String", mode = "IN"),
            @Attribute(name = "inventoryItem", type = "org.ofbiz.entity.GenericValue", mode = "IN"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "quantityIssued", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "finishedProductId", type = "String", mode = "OUT")
        }
    )
    public interface IssueInventoryItemToWorkEffort {}

    /**
     *              Create Inventory for product produced by a Production Run.         
     */
    @Service(
        name = "productionRunProduce",
        engine = "java",
        location = "org.ofbiz.manufacturing.jobshopmgt.ProductionRunServices",
        invoke = "productionRunProduce",
        description = "\n            Create Inventory for product produced by a Production Run.\n        ",
        auth = "true",
        transactionTimeout = "7200",
        attributes = {
            @Attribute(name = "workEffortId", type = "String", mode = "IN"),
            @Attribute(name = "inventoryItemIds", type = "List", mode = "OUT"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "INOUT", optional = "true"),
            @Attribute(name = "quantityUomId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "inventoryItemTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "lotId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "locationSeqId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "createLotIfNeeded", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "autoCreateLot", type = "Boolean", mode = "IN", optional = "true")
        }
    )
    public interface ProductionRunProduce {}

    /**
     *              Create Inventory for product produced by a Production Run and if necessary add declared quantities to tasks (and issue materials, if needed).         
     */
    @Service(
        name = "productionRunDeclareAndProduce",
        engine = "java",
        location = "org.ofbiz.manufacturing.jobshopmgt.ProductionRunServices",
        invoke = "productionRunDeclareAndProduce",
        description = "\n            Create Inventory for product produced by a Production Run and if necessary add declared quantities to tasks (and issue materials, if needed).\n        ",
        auth = "true",
        transactionTimeout = "7200",
        attributes = {
            @Attribute(name = "workEffortId", type = "String", mode = "IN"),
            @Attribute(name = "inventoryItemIds", type = "List", mode = "OUT"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "INOUT"),
            @Attribute(name = "quantityUomId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "inventoryItemTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "locationSeqId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "lotId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "createLotIfNeeded", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "autoCreateLot", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "componentsLocationMap", type = "Map", mode = "IN", optional = "true")
        }
    )
    public interface ProductionRunDeclareAndProduce {}

    /**
     *              Create Inventory from a Production Run Task.         
     */
    @Service(
        name = "productionRunTaskProduce",
        engine = "java",
        location = "org.ofbiz.manufacturing.jobshopmgt.ProductionRunServices",
        invoke = "productionRunTaskProduce",
        description = "\n            Create Inventory from a Production Run Task.\n        ",
        auth = "true",
        attributes = {
            @Attribute(name = "workEffortId", type = "String", mode = "IN"),
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "facilityId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "locationSeqId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "unitCost", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "currencyUomId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "inventoryItemTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "inventoryItemIds", type = "List", mode = "OUT"),
            @Attribute(name = "lotId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "isReturned", type = "String", mode = "IN", optional = "true", defaultValue = "N")
        }
    )
    public interface ProductionRunTaskProduce {}

    /**
     *              Create Inventory from a Production Run Task, by returning to warehouse part of the materials allocated.         
     */
    @Service(
        name = "productionRunTaskReturnMaterial",
        engine = "java",
        location = "org.ofbiz.manufacturing.jobshopmgt.ProductionRunServices",
        invoke = "productionRunTaskReturnMaterial",
        description = "\n            Create Inventory from a Production Run Task, by returning to warehouse part of the materials allocated.\n        ",
        auth = "true",
        attributes = {
            @Attribute(name = "workEffortId", type = "String", mode = "IN"),
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "inventoryItemTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "lotId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface ProductionRunTaskReturnMaterial {}

    /**
     *              If the inventory item is for a 'marketing package' run the decomposeInventoryItem service.             It is intended to be called as seca when a marketing package is received into warehouse (e.g. from a return).         
     */
    @Service(
        name = "checkDecomposeInventoryItem",
        engine = "java",
        location = "org.ofbiz.manufacturing.jobshopmgt.ProductionRunServices",
        invoke = "checkDecomposeInventoryItem",
        description = "\n            If the inventory item is for a 'marketing package' run the decomposeInventoryItem service.\n            It is intended to be called as seca when a marketing package is received into warehouse (e.g. from a return).\n        ",
        auth = "true",
        attributes = {
            @Attribute(name = "inventoryItemId", type = "String", mode = "IN")
        }
    )
    public interface CheckDecomposeInventoryItem {}

    /**
     *              Create a decompose work effort, issue the inventory item (or part of it), and put in warehouse its components.             It is intended to be called when a marketing package is received into warehouse (e.g. from a return).             The components will be returned to inventory at ((Marketing Package Actual Inventory Unit Cost) / (Marketing Package Standard Cost)) * (Component Standard Cost)         
     */
    @Service(
        name = "decomposeInventoryItem",
        engine = "java",
        location = "org.ofbiz.manufacturing.jobshopmgt.ProductionRunServices",
        invoke = "decomposeInventoryItem",
        description = "\n            Create a decompose work effort, issue the inventory item (or part of it), and put in warehouse its components.\n            It is intended to be called when a marketing package is received into warehouse (e.g. from a return).\n            The components will be returned to inventory at ((Marketing Package Actual Inventory Unit Cost) / (Marketing Package Standard Cost)) * (Component Standard Cost)\n        ",
        auth = "true",
        attributes = {
            @Attribute(name = "inventoryItemId", type = "String", mode = "IN"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "inventoryItemIds", type = "List", mode = "OUT")
        }
    )
    public interface DecomposeInventoryItem {}

    /**
     *              Add a TimeEntry for the production run task and updates the relevant fields.         
     */
    @Service(
        name = "updateProductionRunTask",
        engine = "java",
        location = "org.ofbiz.manufacturing.jobshopmgt.ProductionRunServices",
        invoke = "updateProductionRunTask",
        description = "\n            Add a TimeEntry for the production run task and updates the relevant fields.\n        ",
        auth = "true",
        attributes = {
            @Attribute(name = "productionRunId", type = "String", mode = "IN"),
            @Attribute(name = "productionRunTaskId", type = "String", mode = "IN"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "roleTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "thruDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "addQuantityProduced", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "addQuantityRejected", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "addSetupTime", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "addTaskTime", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "comments", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "issueRequiredComponents", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "componentsLocationMap", type = "Map", mode = "IN", optional = "true")
        }
    )
    public interface UpdateProductionRunTask {}

    /**
     * Quick runs a ProductionRun task to the completed status, also issuing components if necessary.
     */
    @Service(
        name = "quickRunProductionRunTask",
        engine = "java",
        location = "org.ofbiz.manufacturing.jobshopmgt.ProductionRunServices",
        invoke = "quickRunProductionRunTask",
        description = "Quick runs a ProductionRun task to the completed status, also issuing components if necessary.",
        auth = "true",
        attributes = {
            @Attribute(name = "productionRunId", type = "String", mode = "IN"),
            @Attribute(name = "taskId", type = "String", mode = "IN")
        }
    )
    public interface QuickRunProductionRunTask {}

    /**
     * Quick runs all the tasks of a ProductionRun to the completed status, also issuing components if necessary.
     */
    @Service(
        name = "quickRunAllProductionRunTasks",
        engine = "java",
        location = "org.ofbiz.manufacturing.jobshopmgt.ProductionRunServices",
        invoke = "quickRunAllProductionRunTasks",
        description = "Quick runs all the tasks of a ProductionRun to the completed status, also issuing components if necessary.",
        auth = "true",
        attributes = {
            @Attribute(name = "productionRunId", type = "String", mode = "IN")
        }
    )
    public interface QuickRunAllProductionRunTasks {}

    /**
     * Quick starts all the tasks of a ProductionRun.
     */
    @Service(
        name = "quickStartAllProductionRunTasks",
        engine = "java",
        location = "org.ofbiz.manufacturing.jobshopmgt.ProductionRunServices",
        invoke = "quickStartAllProductionRunTasks",
        description = "Quick starts all the tasks of a ProductionRun.",
        auth = "true",
        attributes = {
            @Attribute(name = "productionRunId", type = "String", mode = "IN")
        }
    )
    public interface QuickStartAllProductionRunTasks {}

    /**
     * Quick moves a ProductionRun to the passed in status, performing all the needed tasks in the way
     */
    @Service(
        name = "quickChangeProductionRunStatus",
        engine = "java",
        location = "org.ofbiz.manufacturing.jobshopmgt.ProductionRunServices",
        invoke = "quickChangeProductionRunStatus",
        description = "Quick moves a ProductionRun to the passed in status, performing all the needed tasks in the way",
        auth = "true",
        attributes = {
            @Attribute(name = "productionRunId", type = "String", mode = "IN"),
            @Attribute(name = "statusId", type = "String", mode = "IN")
        }
    )
    public interface QuickChangeProductionRunStatus {}

    /**
     * Cancels a ProductionRun.
     */
    @Service(
        name = "cancelProductionRun",
        engine = "java",
        location = "org.ofbiz.manufacturing.jobshopmgt.ProductionRunServices",
        invoke = "cancelProductionRun",
        description = "Cancels a ProductionRun.",
        auth = "true",
        attributes = {
            @Attribute(name = "productionRunId", type = "String", mode = "IN")
        }
    )
    public interface CancelProductionRun {}

    /**
     * Given a productId and an optional date, returns the total qty of productId reserved by production runs
     */
    @Service(
        name = "getProductionRunTotResQty",
        engine = "java",
        location = "org.ofbiz.manufacturing.jobshopmgt.ProductionRunServices",
        invoke = "getProductionRunTotResQty",
        description = "Given a productId and an optional date, returns the total qty of productId reserved by production runs",
        auth = "true",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "startDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "reservedQuantity", type = "BigDecimal", mode = "OUT")
        }
    )
    public interface GetProductionRunTotResQty {}

    /**
     * Retrieve the costs of a work effort (production run task).
     */
    @Service(
        name = "getWorkEffortCosts",
        engine = "java",
        location = "org.ofbiz.manufacturing.jobshopmgt.ProductionRunServices",
        invoke = "getWorkEffortCosts",
        description = "Retrieve the costs of a work effort (production run task).",
        auth = "true",
        attributes = {
            @Attribute(name = "workEffortId", type = "String", mode = "IN"),
            @Attribute(name = "costComponents", type = "List", mode = "OUT"),
            @Attribute(name = "totalCost", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "totalCostNoMaterials", type = "BigDecimal", mode = "OUT")
        }
    )
    public interface GetWorkEffortCosts {}

    /**
     * Retrieve the total cost of a production run.
     */
    @Service(
        name = "getProductionRunCost",
        engine = "java",
        location = "org.ofbiz.manufacturing.jobshopmgt.ProductionRunServices",
        invoke = "getProductionRunCost",
        description = "Retrieve the total cost of a production run.",
        auth = "true",
        attributes = {
            @Attribute(name = "workEffortId", type = "String", mode = "IN"),
            @Attribute(name = "totalCost", type = "BigDecimal", mode = "OUT")
        }
    )
    public interface GetProductionRunCost {}

    /**
     * Compute the actual costs for the production run task.
     */
    @Service(
        name = "createProductionRunTaskCosts",
        engine = "java",
        location = "org.ofbiz.manufacturing.jobshopmgt.ProductionRunServices",
        invoke = "createProductionRunTaskCosts",
        description = "Compute the actual costs for the production run task.",
        auth = "true",
        attributes = {
            @Attribute(name = "productionRunTaskId", type = "String", mode = "IN")
        }
    )
    public interface CreateProductionRunTaskCosts {}

}

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
package com.ilscipio.scipio.order.service;

import com.ilscipio.scipio.service.def.*;
import com.ilscipio.scipio.service.def.Service.GroupInvoke;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class RequirementServices {

    /**
     * Create a new requirement
     */
    @Service(
        name = "createRequirement",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/requirement/RequirementServices.xml",
        invoke = "createRequirement",
        description = "Create a new requirement",
        entityAttributes = {
            @EntityAttributes(entityName = "Requirement", mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "requirementTypeId", type = "String", mode = "IN"),
            @Attribute(name = "custRequestId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "custRequestItemSeqId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "requirementId", type = "String", mode = "INOUT", optional = "true")
        }
    )
    public interface CreateRequirement {}

    /**
     * Update an existing requirement
     */
    @Service(
        name = "updateRequirement",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/requirement/RequirementServices.xml",
        invoke = "updateRequirement",
        description = "Update an existing requirement",
        entityAttributes = {
            @EntityAttributes(entityName = "Requirement", mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "requirementId", type = "String", mode = "IN"),
            @Attribute(name = "oldStatusId", type = "String", mode = "OUT")
        }
    )
    public interface UpdateRequirement {}

    /**
     * Delete a requirement after deleting related entity records.
     */
    @Service(
        name = "deleteRequirement",
        engine = "group",
        description = "Delete a requirement after deleting related entity records.",
        invokes = {@GroupInvoke(name = "deleteRequirementAndRelated", resultToContext = "false")}
    )
    public interface DeleteRequirement {}

    /**
     * Delete a requirement after deleting related entity records.
     */
    @Service(
        name = "deleteRequirementAndRelated",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/requirement/RequirementServices.xml",
        invoke = "deleteRequirementAndRelated",
        description = "Delete a requirement after deleting related entity records.",
        defaultEntityName = "Requirement",
        attributes = {
            @Attribute(name = "requirementId", type = "String", mode = "IN")
        }
    )
    public interface DeleteRequirementAndRelated {}

    /**
     * Delete a requirement, core record only (SCIPIO)
     */
    @Service(
        name = "deleteRequirementOnly",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a requirement, core record only (SCIPIO)",
        defaultEntityName = "Requirement",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteRequirementOnly {}

    /**
     * Creates a new party role for the requirement
     */
    @Service(
        name = "createRequirementRole",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/requirement/RequirementServices.xml",
        invoke = "createRequirementRole",
        description = "Creates a new party role for the requirement",
        defaultEntityName = "RequirementRole",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "true")
        }
    )
    public interface CreateRequirementRole {}

    /**
     * Update a RequirementRole
     */
    @Service(
        name = "updateRequirementRole",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/requirement/RequirementServices.xml",
        invoke = "updateRequirementRole",
        description = "Update a RequirementRole",
        defaultEntityName = "RequirementRole",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateRequirementRole {}

    /**
     * Remove a RequirementRole
     */
    @Service(
        name = "removeRequirementRole",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/requirement/RequirementServices.xml",
        invoke = "removeRequirementRole",
        description = "Remove a RequirementRole",
        defaultEntityName = "RequirementRole",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface RemoveRequirementRole {}

    /**
     * Create Requirement Status
     */
    @Service(
        name = "createRequirementStatus",
        engine = "entity-auto",
        invoke = "create",
        description = "Create Requirement Status",
        defaultEntityName = "RequirementStatus",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateRequirementStatus {}

    /**
     * Creates a CustRequestItem/Requirement association
     */
    @Service(
        name = "associatedRequirementWithRequestItem",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/requirement/RequirementServices.xml",
        invoke = "associateRequirementWithRequestItem",
        description = "Creates a CustRequestItem/Requirement association",
        attributes = {
            @Attribute(name = "requirementId", type = "String", mode = "IN"),
            @Attribute(name = "custRequestId", type = "String", mode = "IN"),
            @Attribute(name = "custRequestItemSeqId", type = "String", mode = "IN")
        }
    )
    public interface AssociatedRequirementWithRequestItem {}

    /**
     * Associate an existing task w/ a requirement
     */
    @Service(
        name = "addRequirementTask",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/requirement/RequirementServices.xml",
        invoke = "addTaskToRequirement",
        description = "Associate an existing task w/ a requirement",
        attributes = {
            @Attribute(name = "requirementId", type = "String", mode = "IN"),
            @Attribute(name = "workEffortId", type = "String", mode = "IN"),
            @Attribute(name = "workReqFulfTypeId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface AddRequirementTask {}

    /**
     * Retrieves requirements information for suppliers
     */
    @Service(
        name = "getRequirementsForSupplier",
        engine = "java",
        location = "org.ofbiz.order.requirement.RequirementServices",
        invoke = "getRequirementsForSupplier",
        description = "Retrieves requirements information for suppliers",
        attributes = {
            @Attribute(name = "requirementConditions", type = "org.ofbiz.entity.condition.EntityCondition", mode = "IN", optional = "true"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "unassignedRequirements", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "currencyUomId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "statusIds", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "requirementsForSupplier", type = "java.util.List", mode = "OUT", optional = "true"),
            @Attribute(name = "distinctProductCount", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "quantityTotal", type = "BigDecimal", mode = "OUT", optional = "true"),
            @Attribute(name = "amountTotal", type = "BigDecimal", mode = "OUT", optional = "true")
        }
    )
    public interface GetRequirementsForSupplier {}

    @Service(
        name = "createOrderRequirementCommitment",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderServices.xml",
        invoke = "createOrderRequirementCommitment",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "OrderRequirementCommitment", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "OrderRequirementCommitment", mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateOrderRequirementCommitment {}

    /**
     * Create OrderRequirementCommitment and Requirement for items with automatic requirement upon ordering
     */
    @Service(
        name = "checkCreateOrderRequirement",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderServices.xml",
        invoke = "checkCreateOrderRequirement",
        description = "Create OrderRequirementCommitment and Requirement for items with automatic requirement upon ordering",
        auth = "true",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "orderItemSeqId", type = "String", mode = "IN"),
            @Attribute(name = "productId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "requirementId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface CheckCreateOrderRequirement {}

    /**
     * Create a Product Requirement based on QOH inventory
     */
    @Service(
        name = "checkCreateStockRequirementQoh",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderServices.xml",
        invoke = "checkCreateStockRequirementQoh",
        description = "Create a Product Requirement based on QOH inventory",
        defaultEntityName = "ItemIssuance",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true", excludeFields = {"issuedDateTime"})
        },
        attributes = {
            @Attribute(name = "requirementId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface CheckCreateStockRequirementQoh {}

    /**
     * Create a Product Requirement based on ATP inventory
     */
    @Service(
        name = "checkCreateStockRequirementAtp",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderServices.xml",
        invoke = "checkCreateStockRequirementAtp",
        description = "Create a Product Requirement based on ATP inventory",
        auth = "true",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "shipGroupSeqId", type = "String", mode = "IN"),
            @Attribute(name = "orderItemSeqId", type = "String", mode = "IN"),
            @Attribute(name = "inventoryItemId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "requirementId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface CheckCreateStockRequirementAtp {}

    /**
     * Create OrderRequirementCommitment and Requirement for items with requirement based on ATP stock levels
     */
    @Service(
        name = "createRequirementFromItemATP",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderServices.xml",
        invoke = "createRequirementFromItemATP",
        description = "Create OrderRequirementCommitment and Requirement for items with requirement based on ATP stock levels",
        auth = "true",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "shipGroupSeqId", type = "String", mode = "IN"),
            @Attribute(name = "orderItemSeqId", type = "String", mode = "IN"),
            @Attribute(name = "inventoryItemId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "requirementId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface CreateRequirementFromItemATP {}

    /**
     * Create Requirements for all the products in a facility with QOH under the minimum stock level
     */
    @Service(
        name = "checkCreateProductRequirementForFacility",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderServices.xml",
        invoke = "checkCreateProductRequirementForFacility",
        description = "Create Requirements for all the products in a facility with QOH under the minimum stock level",
        auth = "true",
        attributes = {
            @Attribute(name = "facilityId", type = "String", mode = "IN"),
            @Attribute(name = "defaultRequirementMethodId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface CheckCreateProductRequirementForFacility {}

    /**
     * Approves a requirement.
     */
    @Service(
        name = "approveRequirement",
        engine = "java",
        location = "org.ofbiz.manufacturing.jobshopmgt.ProductionRunServices",
        invoke = "approveRequirement",
        description = "Approves a requirement.",
        auth = "true",
        attributes = {
            @Attribute(name = "requirementId", type = "String", mode = "IN"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN", optional = "true")
        }
    )
    public interface ApproveRequirement {}

    /**
     * If the requirement is a product requirement (purchasing) try to assign it to the primary supplier
     */
    @Service(
        name = "autoAssignRequirementToSupplier",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/requirement/RequirementServices.xml",
        invoke = "autoAssignRequirementToSupplier",
        description = "If the requirement is a product requirement (purchasing) try to assign it to the primary supplier",
        auth = "true",
        attributes = {
            @Attribute(name = "requirementId", type = "String", mode = "IN")
        }
    )
    public interface AutoAssignRequirementToSupplier {}

    /**
     * Create the inventory transfers required to fulfill the requirement.
     */
    @Service(
        name = "createTransferFromRequirement",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/requirement/RequirementServices.xml",
        invoke = "createTransferFromRequirement",
        description = "Create the inventory transfers required to fulfill the requirement.",
        auth = "true",
        attributes = {
            @Attribute(name = "requirementId", type = "String", mode = "IN"),
            @Attribute(name = "fromFacilityId", type = "String", mode = "IN"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN", optional = "true")
        }
    )
    public interface CreateTransferFromRequirement {}

    /**
     *              Creates requirements for any products with requirementMethodEnumId PRODRQM_AUTO in the given sales order.         
     */
    @Service(
        name = "createAutoRequirementsForOrder",
        engine = "java",
        location = "org.ofbiz.order.requirement.RequirementServices",
        invoke = "createAutoRequirementsForOrder",
        description = "\n            Creates requirements for any products with requirementMethodEnumId PRODRQM_AUTO in the given sales order.\n        ",
        auth = "true",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN")
        }
    )
    public interface CreateAutoRequirementsForOrder {}

    /**
     *              Creates requirements for any products with requirementMethodEnumId PRODRQM_ATP in the given sales order when             the ATP falls below or is below the minimum stock for the order facility.  ProductFacility.minimumStock must             be configured for requirements to be generated.  ProductFacility.reorderQuantity is not currently supported.         
     */
    @Service(
        name = "createATPRequirementsForOrder",
        engine = "java",
        location = "org.ofbiz.order.requirement.RequirementServices",
        invoke = "createATPRequirementsForOrder",
        description = "\n            Creates requirements for any products with requirementMethodEnumId PRODRQM_ATP in the given sales order when\n            the ATP falls below or is below the minimum stock for the order facility.  ProductFacility.minimumStock must\n            be configured for requirements to be generated.  ProductFacility.reorderQuantity is not currently supported.\n        ",
        auth = "true",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN")
        }
    )
    public interface CreateATPRequirementsForOrder {}

    /**
     *              Update requirement's status to Ordered after PO is approved.         
     */
    @Service(
        name = "updateRequirementsToOrdered",
        engine = "java",
        location = "org.ofbiz.order.requirement.RequirementServices",
        invoke = "updateRequirementsToOrdered",
        description = "\n            Update requirement's status to Ordered after PO is approved.\n        ",
        auth = "true",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN")
        }
    )
    public interface UpdateRequirementsToOrdered {}

    /**
     * Create a DesiredFeature record
     */
    @Service(
        name = "createDesiredFeature",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a DesiredFeature record",
        defaultEntityName = "DesiredFeature",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateDesiredFeature {}

    /**
     * Update a DesiredFeature record
     */
    @Service(
        name = "updateDesiredFeature",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a DesiredFeature record",
        defaultEntityName = "DesiredFeature",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateDesiredFeature {}

    /**
     * Delete a DesiredFeature record
     */
    @Service(
        name = "deleteDesiredFeature",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a DesiredFeature record",
        defaultEntityName = "DesiredFeature",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteDesiredFeature {}

    /**
     * Create a RequirementAttribute record
     */
    @Service(
        name = "createRequirementAttribute",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a RequirementAttribute record",
        defaultEntityName = "RequirementAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateRequirementAttribute {}

    /**
     * Update a RequirementAttribute record
     */
    @Service(
        name = "updateRequirementAttribute",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a RequirementAttribute record",
        defaultEntityName = "RequirementAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateRequirementAttribute {}

    /**
     * Delete a RequirementAttribute record
     */
    @Service(
        name = "deleteRequirementAttribute",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a RequirementAttribute record",
        defaultEntityName = "RequirementAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteRequirementAttribute {}

    /**
     * Create a RequirementBudgetAllocation record
     */
    @Service(
        name = "createRequirementBudgetAllocation",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a RequirementBudgetAllocation record",
        defaultEntityName = "RequirementBudgetAllocation",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateRequirementBudgetAllocation {}

    /**
     * Update a RequirementBudgetAllocation record
     */
    @Service(
        name = "updateRequirementBudgetAllocation",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a RequirementBudgetAllocation record",
        defaultEntityName = "RequirementBudgetAllocation",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateRequirementBudgetAllocation {}

    /**
     * Delete a RequirementBudgetAllocation record
     */
    @Service(
        name = "deleteRequirementBudgetAllocation",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a RequirementBudgetAllocation record",
        defaultEntityName = "RequirementBudgetAllocation",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteRequirementBudgetAllocation {}

    /**
     * Create a RequirementType record
     */
    @Service(
        name = "createRequirementType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a RequirementType record",
        defaultEntityName = "RequirementType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateRequirementType {}

    /**
     * Update a RequirementType record
     */
    @Service(
        name = "updateRequirementType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a RequirementType record",
        defaultEntityName = "RequirementType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateRequirementType {}

    /**
     * Delete a RequirementType record
     */
    @Service(
        name = "deleteRequirementType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a RequirementType record",
        defaultEntityName = "RequirementType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteRequirementType {}

    /**
     * Create a RequirementTypeAttr record
     */
    @Service(
        name = "createRequirementTypeAttr",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a RequirementTypeAttr record",
        defaultEntityName = "RequirementTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateRequirementTypeAttr {}

    /**
     * Update a RequirementTypeAttr record
     */
    @Service(
        name = "updateRequirementTypeAttr",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a RequirementTypeAttr record",
        defaultEntityName = "RequirementTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateRequirementTypeAttr {}

    /**
     * Delete a RequirementTypeAttr record
     */
    @Service(
        name = "deleteRequirementTypeAttr",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a RequirementTypeAttr record",
        defaultEntityName = "RequirementTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteRequirementTypeAttr {}

    /**
     * Delete a RequirementCustRequest record
     */
    @Service(
        name = "deleteRequirementCustRequest",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a RequirementCustRequest record",
        defaultEntityName = "RequirementCustRequest",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteRequirementCustRequest {}

    /**
     * Create a WorkReqFulfType record
     */
    @Service(
        name = "createWorkReqFulfType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a WorkReqFulfType record",
        defaultEntityName = "WorkReqFulfType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateWorkReqFulfType {}

    /**
     * Update a WorkReqFulfType record
     */
    @Service(
        name = "updateWorkReqFulfType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a WorkReqFulfType record",
        defaultEntityName = "WorkReqFulfType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateWorkReqFulfType {}

    /**
     * Delete a WorkReqFulfType record
     */
    @Service(
        name = "deleteWorkReqFulfType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a WorkReqFulfType record",
        defaultEntityName = "WorkReqFulfType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteWorkReqFulfType {}

}

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
package com.ilscipio.scipio.accounting.service;

import com.ilscipio.scipio.service.def.*;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class BudgetServices {

    /**
     * Create a Budget
     */
    @Service(
        name = "createBudget",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/budget/BudgetServices.xml",
        invoke = "createBudget",
        description = "Create a Budget",
        defaultEntityName = "Budget",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateBudget {}

    /**
     * Update a Budget
     */
    @Service(
        name = "updateBudget",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Budget",
        defaultEntityName = "Budget",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateBudget {}

    /**
     * Create Budget Status Record
     */
    @Service(
        name = "createBudgetStatus",
        engine = "entity-auto",
        invoke = "create",
        description = "Create Budget Status Record",
        defaultEntityName = "BudgetStatus",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateBudgetStatus {}

    /**
     * Update a Budget
     */
    @Service(
        name = "updateBudgetStatus",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/budget/BudgetServices.xml",
        invoke = "updateBudgetStatus",
        description = "Update a Budget",
        defaultEntityName = "BudgetStatus",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateBudgetStatus {}

    /**
     * Create a Budget Item
     */
    @Service(
        name = "createBudgetItem",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a Budget Item",
        defaultEntityName = "BudgetItem",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "budgetId", type = "String", mode = "IN"),
            @Attribute(name = "budgetItemSeqId", type = "String", mode = "OUT")
        }
    )
    public interface CreateBudgetItem {}

    /**
     * Update a Budget Item
     */
    @Service(
        name = "updateBudgetItem",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Budget Item",
        defaultEntityName = "BudgetItem",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateBudgetItem {}

    /**
     * Remove an existing Budget Item Record
     */
    @Service(
        name = "removeBudgetItem",
        engine = "entity-auto",
        invoke = "delete",
        description = "Remove an existing Budget Item Record",
        defaultEntityName = "BudgetItem",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface RemoveBudgetItem {}

    /**
     * Create a new Budget Role Record
     */
    @Service(
        name = "createBudgetRole",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/budget/BudgetServices.xml",
        invoke = "createBudgetRole",
        description = "Create a new Budget Role Record",
        defaultEntityName = "BudgetRole",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateBudgetRole {}

    /**
     * Remove an existing Budget Role Record
     */
    @Service(
        name = "removeBudgetRole",
        engine = "entity-auto",
        invoke = "delete",
        description = "Remove an existing Budget Role Record",
        defaultEntityName = "BudgetRole",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface RemoveBudgetRole {}

    /**
     * Create a new Budget Review Record
     */
    @Service(
        name = "createBudgetReview",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a new Budget Review Record",
        defaultEntityName = "BudgetReview",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "budgetReviewId", type = "String", mode = "OUT", optional = "false")
        }
    )
    public interface CreateBudgetReview {}

    /**
     * Remove an existing Budget Review Record
     */
    @Service(
        name = "removeBudgetReview",
        engine = "entity-auto",
        invoke = "delete",
        description = "Remove an existing Budget Review Record",
        defaultEntityName = "BudgetReview",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface RemoveBudgetReview {}

    /**
     * Create a Budget Type Record
     */
    @Service(
        name = "createBudgetType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a Budget Type Record",
        defaultEntityName = "BudgetType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateBudgetType {}

    /**
     * Update a Budget Type Record
     */
    @Service(
        name = "updateBudgetType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Budget Type Record",
        defaultEntityName = "BudgetType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateBudgetType {}

    /**
     * Delete a Budget Type Record
     */
    @Service(
        name = "deleteBudgetType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a Budget Type Record",
        defaultEntityName = "BudgetType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface DeleteBudgetType {}

    /**
     * Create a new Budget Item Attribute Record
     */
    @Service(
        name = "createBudgetItemAttribute",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a new Budget Item Attribute Record",
        defaultEntityName = "BudgetItemAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateBudgetItemAttribute {}

    /**
     * Update a Budget Item Attribute Record
     */
    @Service(
        name = "updateBudgetItemAttribute",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Budget Item Attribute Record",
        defaultEntityName = "BudgetItemAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateBudgetItemAttribute {}

    /**
     * Delete an existing Budget Item Attribute Record
     */
    @Service(
        name = "deleteBudgetItemAttribute",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete an existing Budget Item Attribute Record",
        defaultEntityName = "BudgetItemAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteBudgetItemAttribute {}

    /**
     * Create a new Budget Item Type Record
     */
    @Service(
        name = "createBudgetItemType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a new Budget Item Type Record",
        defaultEntityName = "BudgetItemType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateBudgetItemType {}

    /**
     * Update a Budget Item Type Record
     */
    @Service(
        name = "updateBudgetItemType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Budget Item Type Record",
        defaultEntityName = "BudgetItemType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateBudgetItemType {}

    /**
     * Remove an existing Budget Item Type Record
     */
    @Service(
        name = "removeBudgetItemType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Remove an existing Budget Item Type Record",
        defaultEntityName = "BudgetItemType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface RemoveBudgetItemType {}

    /**
     * Create a new Budget Item Type Attr Record
     */
    @Service(
        name = "createBudgetItemTypeAttr",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a new Budget Item Type Attr Record",
        defaultEntityName = "BudgetItemTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateBudgetItemTypeAttr {}

    /**
     * Update a Budget Item Type Attr Record
     */
    @Service(
        name = "updateBudgetItemTypeAttr",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Budget Item Type Attr Record",
        defaultEntityName = "BudgetItemTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateBudgetItemTypeAttr {}

    /**
     * Delete an existing Budget Item Type Attr Record
     */
    @Service(
        name = "deleteBudgetItemTypeAttr",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete an existing Budget Item Type Attr Record",
        defaultEntityName = "BudgetItemTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteBudgetItemTypeAttr {}

    /**
     * Create a new Budget Review Result Type Record
     */
    @Service(
        name = "createBudgetReviewResultType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a new Budget Review Result Type Record",
        defaultEntityName = "BudgetReviewResultType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateBudgetReviewResultType {}

    /**
     * Update a Budget Review Result Type Record
     */
    @Service(
        name = "updateBudgetReviewResultType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Budget Review Result Type Record",
        defaultEntityName = "BudgetReviewResultType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateBudgetReviewResultType {}

    /**
     * Delete an existing Budget Review Result Type Record
     */
    @Service(
        name = "deleteBudgetReviewResultType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete an existing Budget Review Result Type Record",
        defaultEntityName = "BudgetReviewResultType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteBudgetReviewResultType {}

    /**
     * Create a new Budget Revision Record
     */
    @Service(
        name = "createBudgetRevision",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a new Budget Revision Record",
        defaultEntityName = "BudgetRevision",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateBudgetRevision {}

    /**
     * Update a Budget Revision Record
     */
    @Service(
        name = "updateBudgetRevision",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Budget Revision Record",
        defaultEntityName = "BudgetRevision",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateBudgetRevision {}

    /**
     * Delete an existing Budget Revision Record. BudgetRevision entity contains historical data, hence this service ideally will not +     be used.
     */
    @Service(
        name = "deleteBudgetRevision",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete an existing Budget Revision Record. BudgetRevision entity contains historical data, hence this service ideally will not +     be used.",
        defaultEntityName = "BudgetRevision",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteBudgetRevision {}

    /**
     * Create a Budget Scenario Record
     */
    @Service(
        name = "createBudgetScenario",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a Budget Scenario Record",
        defaultEntityName = "BudgetScenario",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateBudgetScenario {}

    /**
     * Update a Budget Scenario Record
     */
    @Service(
        name = "updateBudgetScenario",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Budget Scenario Record",
        defaultEntityName = "BudgetScenario",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateBudgetScenario {}

    /**
     * Delete a Budget Scenario Record
     */
    @Service(
        name = "deleteBudgetScenario",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a Budget Scenario Record",
        defaultEntityName = "BudgetScenario",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface DeleteBudgetScenario {}

    /**
     * Create a BudgetRevisionImpact
     */
    @Service(
        name = "createBudgetRevisionImpact",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a BudgetRevisionImpact",
        defaultEntityName = "BudgetRevisionImpact",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface CreateBudgetRevisionImpact {}

    /**
     * Update a BudgetRevisionImpact
     */
    @Service(
        name = "updateBudgetRevisionImpact",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a BudgetRevisionImpact",
        defaultEntityName = "BudgetRevisionImpact",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateBudgetRevisionImpact {}

    /**
     * Remove a BudgetRevisionImpact
     */
    @Service(
        name = "removeBudgetRevisionImpact",
        engine = "entity-auto",
        invoke = "delete",
        description = "Remove a BudgetRevisionImpact",
        defaultEntityName = "BudgetRevisionImpact",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface RemoveBudgetRevisionImpact {}

    /**
     * Create a new Budget Scenario Rule Record
     */
    @Service(
        name = "createBudgetScenarioRule",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a new Budget Scenario Rule Record",
        defaultEntityName = "BudgetScenarioRule",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateBudgetScenarioRule {}

    /**
     * Update a Budget Scenario Rule
     */
    @Service(
        name = "updateBudgetScenarioRule",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Budget Scenario Rule",
        defaultEntityName = "BudgetScenarioRule",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateBudgetScenarioRule {}

    /**
     * Delete an existing Budget Scenario Rule Record
     */
    @Service(
        name = "deleteBudgetScenarioRule",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete an existing Budget Scenario Rule Record",
        defaultEntityName = "BudgetScenarioRule",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteBudgetScenarioRule {}

    /**
     * Create a new BudgetAttribute record
     */
    @Service(
        name = "createBudgetAttribute",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a new BudgetAttribute record",
        defaultEntityName = "BudgetAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateBudgetAttribute {}

    /**
     * Update a BudgetAttribute record
     */
    @Service(
        name = "updateBudgetAttribute",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a BudgetAttribute record",
        defaultEntityName = "BudgetAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateBudgetAttribute {}

    /**
     * Delete a BudgetAttribute record
     */
    @Service(
        name = "removeBudgetAttribute",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a BudgetAttribute record",
        defaultEntityName = "BudgetAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface RemoveBudgetAttribute {}

    /**
     * Create a Budget Type Attr Record
     */
    @Service(
        name = "createBudgetTypeAttr",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a Budget Type Attr Record",
        defaultEntityName = "BudgetTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateBudgetTypeAttr {}

    /**
     * Update a Budget Type Attr Record
     */
    @Service(
        name = "updateBudgetTypeAttr",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Budget Type Attr Record",
        defaultEntityName = "BudgetTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateBudgetTypeAttr {}

    /**
     * Delete a Budget Type Attr Record
     */
    @Service(
        name = "deleteBudgetTypeAttr",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a Budget Type Attr Record",
        defaultEntityName = "BudgetTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteBudgetTypeAttr {}

    /**
     * Create a Budget Scenario Application Record
     */
    @Service(
        name = "createBudgetScenarioApplication",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a Budget Scenario Application Record",
        defaultEntityName = "BudgetScenarioApplication",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateBudgetScenarioApplication {}

    /**
     * Update a Budget Scenario Application Record
     */
    @Service(
        name = "updateBudgetScenarioApplication",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Budget Scenario Application Record",
        defaultEntityName = "BudgetScenarioApplication",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateBudgetScenarioApplication {}

    /**
     * Delete a Budget Scenario Application Record
     */
    @Service(
        name = "deleteBudgetScenarioApplication",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a Budget Scenario Application Record",
        defaultEntityName = "BudgetScenarioApplication",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteBudgetScenarioApplication {}

}

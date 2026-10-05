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
package com.ilscipio.scipio.humanres.service;

import com.ilscipio.scipio.service.def.*;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class Services {

    /**
     * Create a Party Qualification entry
     */
    @Service(
        name = "createPartyQual",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a Party Qualification entry",
        defaultEntityName = "PartyQual",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "partyQualTypeId", type = "String", mode = "IN"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "INOUT", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "CREATE")
    )
    public interface CreatePartyQual {}

    /**
     * Update Qualification of Party
     */
    @Service(
        name = "updatePartyQual",
        engine = "entity-auto",
        invoke = "update",
        description = "Update Qualification of Party",
        defaultEntityName = "PartyQual",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdatePartyQual {}

    /**
     * Delete Qualification of Party
     */
    @Service(
        name = "deletePartyQual",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete Qualification of Party",
        defaultEntityName = "PartyQual",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "DELETE")
    )
    public interface DeletePartyQual {}

    /**
     * Create Resume for a Party
     */
    @Service(
        name = "createPartyResume",
        engine = "entity-auto",
        invoke = "create",
        description = "Create Resume for a Party",
        defaultEntityName = "PartyResume",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "CREATE")
    )
    public interface CreatePartyResume {}

    /**
     * Update a Resume of Party
     */
    @Service(
        name = "updatePartyResume",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Resume of Party",
        defaultEntityName = "PartyResume",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdatePartyResume {}

    /**
     * Delete a Resume of Party
     */
    @Service(
        name = "deletePartyResume",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a Resume of Party",
        defaultEntityName = "PartyResume",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "DELETE")
    )
    public interface DeletePartyResume {}

    /**
     * Create Skill for a Party
     */
    @Service(
        name = "createPartySkill",
        engine = "simple",
        location = "component://humanres/script/org/ofbiz/humanres/HumanResServices.xml",
        invoke = "createPartySkill",
        description = "Create Skill for a Party",
        defaultEntityName = "PartySkill",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "CREATE")
    )
    public interface CreatePartySkill {}

    /**
     * Update a PartySkill
     */
    @Service(
        name = "updatePartySkill",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a PartySkill",
        defaultEntityName = "PartySkill",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdatePartySkill {}

    /**
     * Delete a PartySkill
     */
    @Service(
        name = "deletePartySkill",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a PartySkill",
        defaultEntityName = "PartySkill",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "DELETE")
    )
    public interface DeletePartySkill {}

    /**
     * Create an Performance Review
     */
    @Service(
        name = "createPerfReview",
        engine = "entity-auto",
        invoke = "create",
        description = "Create an Performance Review",
        defaultEntityName = "PerfReview",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "perfReviewId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "perfReviewId", type = "String", mode = "OUT"),
            @Attribute(name = "employeePartyId", type = "String", mode = "INOUT"),
            @Attribute(name = "employeeRoleTypeId", type = "String", mode = "INOUT")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "CREATE")
    )
    public interface CreatePerfReview {}

    /**
     * Update a Performance Review
     */
    @Service(
        name = "updatePerfReview",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Performance Review",
        defaultEntityName = "PerfReview",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdatePerfReview {}

    /**
     * Delete a Performance Review
     */
    @Service(
        name = "deletePerfReview",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a Performance Review",
        defaultEntityName = "PerfReview",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "DELETE")
    )
    public interface DeletePerfReview {}

    /**
     * Create Performance Review Item
     */
    @Service(
        name = "createPerfReviewItem",
        engine = "entity-auto",
        invoke = "create",
        description = "Create Performance Review Item",
        defaultEntityName = "PerfReviewItem",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "employeePartyId", type = "String", mode = "IN"),
            @Attribute(name = "employeeRoleTypeId", type = "String", mode = "IN"),
            @Attribute(name = "perfReviewId", type = "String", mode = "IN"),
            @Attribute(name = "perfReviewItemSeqId", type = "String", mode = "OUT")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "CREATE")
    )
    public interface CreatePerfReviewItem {}

    /**
     * Update a Performance Review Item
     */
    @Service(
        name = "updatePerfReviewItem",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Performance Review Item",
        defaultEntityName = "PerfReviewItem",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdatePerfReviewItem {}

    /**
     * Delete a Performance Review Item
     */
    @Service(
        name = "deletePerfReviewItem",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a Performance Review Item",
        defaultEntityName = "PerfReviewItem",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "DELETE")
    )
    public interface DeletePerfReviewItem {}

    /**
     * Create Performance Note
     */
    @Service(
        name = "createPerformanceNote",
        engine = "entity-auto",
        invoke = "create",
        description = "Create Performance Note",
        defaultEntityName = "PerformanceNote",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", type = "Timestamp", mode = "IN", optional = "true")
        }
    )
    public interface CreatePerformanceNote {}

    /**
     * Update a Performance Note
     */
    @Service(
        name = "updatePerformanceNote",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Performance Note",
        defaultEntityName = "PerformanceNote",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdatePerformanceNote {}

    /**
     * Delete a Performance Note
     */
    @Service(
        name = "deletePerformanceNote",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a Performance Note",
        defaultEntityName = "PerformanceNote",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "DELETE")
    )
    public interface DeletePerformanceNote {}

    /**
     * Create Employment
     */
    @Service(
        name = "createEmployment",
        engine = "entity-auto",
        invoke = "create",
        description = "Create Employment",
        defaultEntityName = "Employment",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", type = "Timestamp", mode = "IN", optional = "true")
        }
    )
    public interface CreateEmployment {}

    /**
     * Update an Employment
     */
    @Service(
        name = "updateEmployment",
        engine = "entity-auto",
        invoke = "update",
        description = "Update an Employment",
        defaultEntityName = "Employment",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdateEmployment {}

    /**
     * Delete an Employment
     */
    @Service(
        name = "deleteEmployment",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete an Employment",
        defaultEntityName = "Employment",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "DELETE")
    )
    public interface DeleteEmployment {}

    /**
     * Create an Employment Application
     */
    @Service(
        name = "createEmploymentApp",
        engine = "simple",
        location = "component://humanres/script/org/ofbiz/humanres/HumanResServices.xml",
        invoke = "createEmploymentApp",
        description = "Create an Employment Application",
        defaultEntityName = "EmploymentApp",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "CREATE")
    )
    public interface CreateEmploymentApp {}

    /**
     * Update an Employment Application
     */
    @Service(
        name = "updateEmploymentApp",
        engine = "simple",
        location = "component://humanres/script/org/ofbiz/humanres/HumanResServices.xml",
        invoke = "updateEmploymentApp",
        description = "Update an Employment Application",
        defaultEntityName = "EmploymentApp",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdateEmploymentApp {}

    /**
     * Delete an Employment Application
     */
    @Service(
        name = "deleteEmploymentApp",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete an Employment Application",
        defaultEntityName = "EmploymentApp",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "DELETE")
    )
    public interface DeleteEmploymentApp {}

    /**
     * Create Party Benefit
     */
    @Service(
        name = "createPartyBenefit",
        engine = "entity-auto",
        invoke = "create",
        description = "Create Party Benefit",
        defaultEntityName = "PartyBenefit",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", type = "Timestamp", mode = "IN", optional = "true")
        }
    )
    public interface CreatePartyBenefit {}

    /**
     * Update Party Benefit
     */
    @Service(
        name = "updatePartyBenefit",
        engine = "entity-auto",
        invoke = "update",
        description = "Update Party Benefit",
        defaultEntityName = "PartyBenefit",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdatePartyBenefit {}

    /**
     * Delete Party Benefit
     */
    @Service(
        name = "deletePartyBenefit",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete Party Benefit",
        defaultEntityName = "PartyBenefit",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "DELETE")
    )
    public interface DeletePartyBenefit {}

    /**
     * Create a Pay Grade
     */
    @Service(
        name = "createPayGrade",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a Pay Grade",
        defaultEntityName = "PayGrade",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "payGradeName", optional = "false")
        }
    )
    public interface CreatePayGrade {}

    /**
     * Update a Pay Grade
     */
    @Service(
        name = "updatePayGrade",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Pay Grade",
        defaultEntityName = "PayGrade",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "payGradeName", optional = "false")
        }
    )
    public interface UpdatePayGrade {}

    /**
     * Delete a Pay Grade
     */
    @Service(
        name = "deletePayGrade",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a Pay Grade",
        defaultEntityName = "PayGrade",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "DELETE")
    )
    public interface DeletePayGrade {}

    /**
     * Create Pay History
     */
    @Service(
        name = "createPayHistory",
        engine = "entity-auto",
        invoke = "create",
        description = "Create Pay History",
        defaultEntityName = "PayHistory",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", type = "Timestamp", mode = "IN", optional = "true")
        }
    )
    public interface CreatePayHistory {}

    /**
     * Update Pay History
     */
    @Service(
        name = "updatePayHistory",
        engine = "entity-auto",
        invoke = "update",
        description = "Update Pay History",
        defaultEntityName = "PayHistory",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdatePayHistory {}

    /**
     * Delete Pay History
     */
    @Service(
        name = "deletePayHistory",
        engine = "simple",
        location = "component://humanres/script/org/ofbiz/humanres/HumanResServices.xml",
        invoke = "deletePayHistory",
        description = "Delete Pay History",
        defaultEntityName = "PayHistory",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "DELETE")
    )
    public interface DeletePayHistory {}

    /**
     * Expire Pay History
     */
    @Service(
        name = "expirePayHistory",
        engine = "entity-auto",
        invoke = "expire",
        description = "Expire Pay History",
        defaultEntityName = "PayHistory",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "UPDATE")
    )
    public interface ExpirePayHistory {}

    /**
     * Create Payroll Preference
     */
    @Service(
        name = "createPayrollPreference",
        engine = "entity-auto",
        invoke = "create",
        description = "Create Payroll Preference",
        defaultEntityName = "PayrollPreference",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "payrollPreferenceSeqId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface CreatePayrollPreference {}

    /**
     * Update Payroll Preference
     */
    @Service(
        name = "updatePayrollPreference",
        engine = "entity-auto",
        invoke = "update",
        description = "Update Payroll Preference",
        defaultEntityName = "PayrollPreference",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdatePayrollPreference {}

    /**
     * Delete Payroll Preference
     */
    @Service(
        name = "deletePayrollPreference",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete Payroll Preference",
        defaultEntityName = "PayrollPreference",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "DELETE")
    )
    public interface DeletePayrollPreference {}

    /**
     * Create Salary Step
     */
    @Service(
        name = "createSalaryStep",
        engine = "simple",
        location = "component://humanres/script/org/ofbiz/humanres/HumanResServices.xml",
        invoke = "createSalaryStep",
        description = "Create Salary Step",
        defaultEntityName = "SalaryStep",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", excludeFields = {"salaryStepSeqId"}),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "salaryStepSeqId", type = "String", mode = "OUT")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "CREATE")
    )
    public interface CreateSalaryStep {}

    /**
     * Update Salary Step
     */
    @Service(
        name = "updateSalaryStep",
        engine = "simple",
        location = "component://humanres/script/org/ofbiz/humanres/HumanResServices.xml",
        invoke = "updateSalaryStep",
        description = "Update Salary Step",
        defaultEntityName = "SalaryStep",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdateSalaryStep {}

    /**
     * Delete Salary Step
     */
    @Service(
        name = "deleteSalaryStep",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete Salary Step",
        defaultEntityName = "SalaryStep",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "DELETE")
    )
    public interface DeleteSalaryStep {}

    /**
     * Create an Termination Reason
     */
    @Service(
        name = "createTerminationReason",
        engine = "entity-auto",
        invoke = "create",
        description = "Create an Termination Reason",
        defaultEntityName = "TerminationReason",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "description", optional = "false")
        }
    )
    public interface CreateTerminationReason {}

    /**
     * Update an Termination Reason
     */
    @Service(
        name = "updateTerminationReason",
        engine = "entity-auto",
        invoke = "update",
        description = "Update an Termination Reason",
        defaultEntityName = "TerminationReason",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "description", optional = "false")
        }
    )
    public interface UpdateTerminationReason {}

    /**
     * Delete an Termination Reason
     */
    @Service(
        name = "deleteTerminationReason",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete an Termination Reason",
        defaultEntityName = "TerminationReason",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "DELETE")
    )
    public interface DeleteTerminationReason {}

    /**
     * Create an Unemployment Claim
     */
    @Service(
        name = "createUnemploymentClaim",
        engine = "entity-auto",
        invoke = "create",
        description = "Create an Unemployment Claim",
        defaultEntityName = "UnemploymentClaim",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "CREATE")
    )
    public interface CreateUnemploymentClaim {}

    /**
     * Update an Unemployment Claim
     */
    @Service(
        name = "updateUnemploymentClaim",
        engine = "entity-auto",
        invoke = "update",
        description = "Update an Unemployment Claim",
        defaultEntityName = "UnemploymentClaim",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdateUnemploymentClaim {}

    /**
     * Delete an Unemployment Claim
     */
    @Service(
        name = "deleteUnemploymentClaim",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete an Unemployment Claim",
        defaultEntityName = "UnemploymentClaim",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "DELETE")
    )
    public interface DeleteUnemploymentClaim {}

    /**
     * Create an Employee Position
     */
    @Service(
        name = "createEmplPosition",
        engine = "entity-auto",
        invoke = "create",
        description = "Create an Employee Position",
        defaultEntityName = "EmplPosition",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "CREATE")
    )
    public interface CreateEmplPosition {}

    /**
     * Update an Employee Position
     */
    @Service(
        name = "updateEmplPosition",
        engine = "entity-auto",
        invoke = "update",
        description = "Update an Employee Position",
        defaultEntityName = "EmplPosition",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdateEmplPosition {}

    /**
     * Delete an Employee Position
     */
    @Service(
        name = "deleteEmplPosition",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete an Employee Position",
        defaultEntityName = "EmplPosition",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "DELETE")
    )
    public interface DeleteEmplPosition {}

    /**
     * Create Employee Position Fulfillment
     */
    @Service(
        name = "createEmplPositionFulfillment",
        engine = "entity-auto",
        invoke = "create",
        description = "Create Employee Position Fulfillment",
        defaultEntityName = "EmplPositionFulfillment",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", type = "Timestamp", mode = "IN", optional = "true")
        }
    )
    public interface CreateEmplPositionFulfillment {}

    /**
     * Update Employee Position Fulfillment
     */
    @Service(
        name = "updateEmplPositionFulfillment",
        engine = "entity-auto",
        invoke = "update",
        description = "Update Employee Position Fulfillment",
        defaultEntityName = "EmplPositionFulfillment",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdateEmplPositionFulfillment {}

    /**
     * Delete Employee Position Fulfillment
     */
    @Service(
        name = "deleteEmplPositionFulfillment",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete Employee Position Fulfillment",
        defaultEntityName = "EmplPositionFulfillment",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "DELETE")
    )
    public interface DeleteEmplPositionFulfillment {}

    /**
     * Create Employee Position Reporting Structure
     */
    @Service(
        name = "createEmplPositionReportingStruct",
        engine = "simple",
        location = "component://humanres/script/org/ofbiz/humanres/HumanResServices.xml",
        invoke = "createEmplPositionReportingStruct",
        description = "Create Employee Position Reporting Structure",
        defaultEntityName = "EmplPositionReportingStruct",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", type = "Timestamp", mode = "IN", optional = "true")
        }
    )
    public interface CreateEmplPositionReportingStruct {}

    /**
     * Update Employee Position Reporting Structure
     */
    @Service(
        name = "updateEmplPositionReportingStruct",
        engine = "entity-auto",
        invoke = "update",
        description = "Update Employee Position Reporting Structure",
        defaultEntityName = "EmplPositionReportingStruct",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdateEmplPositionReportingStruct {}

    /**
     * Delete Employee Position Reporting Structure
     */
    @Service(
        name = "deleteEmplPositionReportingStruct",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete Employee Position Reporting Structure",
        defaultEntityName = "EmplPositionReportingStruct",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "DELETE")
    )
    public interface DeleteEmplPositionReportingStruct {}

    /**
     * Create Employee Position Responsibility
     */
    @Service(
        name = "createEmplPositionResponsibility",
        engine = "entity-auto",
        invoke = "create",
        description = "Create Employee Position Responsibility",
        defaultEntityName = "EmplPositionResponsibility",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", type = "Timestamp", mode = "IN", optional = "true")
        }
    )
    public interface CreateEmplPositionResponsibility {}

    /**
     * Update Employee Position Responsibility
     */
    @Service(
        name = "updateEmplPositionResponsibility",
        engine = "entity-auto",
        invoke = "update",
        description = "Update Employee Position Responsibility",
        defaultEntityName = "EmplPositionResponsibility",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdateEmplPositionResponsibility {}

    /**
     * Delete Employee Position Responsibility
     */
    @Service(
        name = "deleteEmplPositionResponsibility",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete Employee Position Responsibility",
        defaultEntityName = "EmplPositionResponsibility",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "DELETE")
    )
    public interface DeleteEmplPositionResponsibility {}

    /**
     * Create Valid Responsibility
     */
    @Service(
        name = "createValidResponsibility",
        engine = "entity-auto",
        invoke = "create",
        description = "Create Valid Responsibility",
        defaultEntityName = "ValidResponsibility",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", type = "Timestamp", mode = "IN", optional = "true")
        }
    )
    public interface CreateValidResponsibility {}

    /**
     * Update Valid Responsibility
     */
    @Service(
        name = "updateValidResponsibility",
        engine = "entity-auto",
        invoke = "update",
        description = "Update Valid Responsibility",
        defaultEntityName = "ValidResponsibility",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdateValidResponsibility {}

    /**
     * Delete Valid Responsibility
     */
    @Service(
        name = "deleteValidResponsibility",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete Valid Responsibility",
        defaultEntityName = "ValidResponsibility",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "DELETE")
    )
    public interface DeleteValidResponsibility {}

    @Service(
        name = "humanResManagerPermission",
        engine = "simple",
        location = "component://humanres/script/org/ofbiz/humanres/permission/HumanResPermissionServices.xml",
        invoke = "humanResManagerPermission",
        auth = "true",
        implemented = {@Implements(service = "permissionInterface")}
    )
    public interface HumanResManagerPermission {}

    /**
     * Create Valid SkillType
     */
    @Service(
        name = "createSkillType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create Valid SkillType",
        defaultEntityName = "SkillType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "description", optional = "false")
        }
    )
    public interface CreateSkillType {}

    /**
     * Update Valid SkillType
     */
    @Service(
        name = "updateSkillType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update Valid SkillType",
        defaultEntityName = "SkillType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "description", optional = "false")
        }
    )
    public interface UpdateSkillType {}

    /**
     * Delete Valid SkillType
     */
    @Service(
        name = "deleteSkillType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete Valid SkillType",
        defaultEntityName = "SkillType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "DELETE")
    )
    public interface DeleteSkillType {}

    /**
     * Create an Employee its role and contact details
     */
    @Service(
        name = "createEmployee",
        engine = "simple",
        location = "component://humanres/script/org/ofbiz/humanres/HumanResServices.xml",
        invoke = "createEmployee",
        description = "Create an Employee its role and contact details",
        entityAttributes = {
            @EntityAttributes(entityName = "Person", mode = "IN", optional = "true", excludeFields = {"partyId"}),
            @EntityAttributes(entityName = "PostalAddress", mode = "IN", optional = "true", excludeFields = {"contactMechId"}),
            @EntityAttributes(entityName = "TelecomNumber", mode = "IN", optional = "true", excludeFields = {"contactMechId"})
        },
        attributes = {
            @Attribute(name = "emailAddress", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fromDate", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "postalAddContactMechPurpTypeId", type = "String", mode = "IN"),
            @Attribute(name = "partyId", type = "String", mode = "OUT")
        }
    )
    public interface CreateEmployee {}

    /**
     * Create Valid ResponsibilityType
     */
    @Service(
        name = "createResponsibilityType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create Valid ResponsibilityType",
        defaultEntityName = "ResponsibilityType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "description", optional = "false")
        }
    )
    public interface CreateResponsibilityType {}

    /**
     * Update Valid ResponsibilityType
     */
    @Service(
        name = "updateResponsibilityType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update Valid ResponsibilityType",
        defaultEntityName = "ResponsibilityType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "description", optional = "false")
        }
    )
    public interface UpdateResponsibilityType {}

    /**
     * Delete Valid ResponsibilityTrype
     */
    @Service(
        name = "deleteResponsibilityType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete Valid ResponsibilityTrype",
        defaultEntityName = "ResponsibilityType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "DELETE")
    )
    public interface DeleteResponsibilityType {}

    /**
     * Create Valid TerminationType
     */
    @Service(
        name = "createTerminationType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create Valid TerminationType",
        defaultEntityName = "TerminationType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "description", optional = "false")
        }
    )
    public interface CreateTerminationType {}

    /**
     * Update Valid TerminationType
     */
    @Service(
        name = "updateTerminationType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update Valid TerminationType",
        defaultEntityName = "TerminationType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "description", optional = "false")
        }
    )
    public interface UpdateTerminationType {}

    /**
     * Delete Valid TerminationType
     */
    @Service(
        name = "deleteTerminationType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete Valid TerminationType",
        defaultEntityName = "TerminationType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "DELETE")
    )
    public interface DeleteTerminationType {}

    /**
     * Create Valid PositionType
     */
    @Service(
        name = "createEmplPositionType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create Valid PositionType",
        defaultEntityName = "EmplPositionType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "description", optional = "false")
        }
    )
    public interface CreateEmplPositionType {}

    /**
     * Update Valid PositionType
     */
    @Service(
        name = "updateEmplPositionType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update Valid PositionType",
        defaultEntityName = "EmplPositionType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "description", optional = "false")
        }
    )
    public interface UpdateEmplPositionType {}

    /**
     * Delete EmplPositionType
     */
    @Service(
        name = "deleteEmplPositionType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete EmplPositionType",
        defaultEntityName = "EmplPositionType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "DELETE")
    )
    public interface DeleteEmplPositionType {}

    /**
     * Update Valid EmplPositionTypeRate
     */
    @Service(
        name = "updateEmplPositionTypeRate",
        engine = "simple",
        location = "component://humanres/script/org/ofbiz/humanres/HumanResServices.xml",
        invoke = "updateEmplPositionTypeRate",
        description = "Update Valid EmplPositionTypeRate",
        defaultEntityName = "EmplPositionTypeRate",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "rateAmount", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "rateCurrencyUomId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "periodTypeId", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "true")
        }
    )
    public interface UpdateEmplPositionTypeRate {}

    /**
     * Delete Valid EmplPositionTypeRate
     */
    @Service(
        name = "deleteEmplPositionTypeRate",
        engine = "simple",
        location = "component://humanres/script/org/ofbiz/humanres/HumanResServices.xml",
        invoke = "deleteEmplPositionTypeRate",
        description = "Delete Valid EmplPositionTypeRate",
        defaultEntityName = "EmplPositionTypeRate",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "rateAmountFromDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "periodTypeId", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "DELETE")
    )
    public interface DeleteEmplPositionTypeRate {}

    /**
     * Create Agreement Employment Appl
     */
    @Service(
        name = "createAgreementEmploymentAppl",
        engine = "entity-auto",
        invoke = "create",
        description = "Create Agreement Employment Appl",
        defaultEntityName = "AgreementEmploymentAppl",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "DELETE")
    )
    public interface CreateAgreementEmploymentAppl {}

    /**
     * Update Valid AgreementEmploymentAppl
     */
    @Service(
        name = "updateAgreementEmploymentAppl",
        engine = "entity-auto",
        invoke = "update",
        description = "Update Valid AgreementEmploymentAppl",
        defaultEntityName = "AgreementEmploymentAppl",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdateAgreementEmploymentAppl {}

    /**
     * Delete AgreementEmploymentAppl
     */
    @Service(
        name = "deleteAgreementEmploymentAppl",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete AgreementEmploymentAppl",
        defaultEntityName = "AgreementEmploymentAppl",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "DELETE")
    )
    public interface DeleteAgreementEmploymentAppl {}

    /**
     * Create Employee Leave
     */
    @Service(
        name = "createEmplLeave",
        engine = "simple",
        location = "component://humanres/script/org/ofbiz/humanres/HumanResServices.xml",
        invoke = "createEmplLeave",
        description = "Create Employee Leave",
        defaultEntityName = "EmplLeave",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "approverPartyId", optional = "false"),
            @OverrideAttribute(name = "thruDate", optional = "false")
        }
    )
    public interface CreateEmplLeave {}

    /**
     * Update Valid Employee Leave
     */
    @Service(
        name = "updateEmplLeave",
        engine = "entity-auto",
        invoke = "update",
        description = "Update Valid Employee Leave",
        defaultEntityName = "EmplLeave",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "approverPartyId", optional = "false"),
            @OverrideAttribute(name = "thruDate", optional = "false")
        }
    )
    public interface UpdateEmplLeave {}

    /**
     * Delete AgreementEmploymentAppl
     */
    @Service(
        name = "deleteEmplLeave",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete AgreementEmploymentAppl",
        defaultEntityName = "EmplLeave",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "DELETE")
    )
    public interface DeleteEmplLeave {}

    /**
     * Create Valid LeaveType
     */
    @Service(
        name = "createEmplLeaveType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create Valid LeaveType",
        defaultEntityName = "EmplLeaveType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "description", optional = "false")
        }
    )
    public interface CreateEmplLeaveType {}

    /**
     * Update Valid LeaveType
     */
    @Service(
        name = "updateEmplLeaveType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update Valid LeaveType",
        defaultEntityName = "EmplLeaveType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "description", optional = "false")
        }
    )
    public interface UpdateEmplLeaveType {}

    /**
     * Delete Valid LeaveType
     */
    @Service(
        name = "deleteEmplLeaveType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete Valid LeaveType",
        defaultEntityName = "EmplLeaveType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "DELETE")
    )
    public interface DeleteEmplLeaveType {}

    /**
     * Delete Valid LeaveType
     */
    @Service(
        name = "getCurrentPartyEmploymentData",
        engine = "simple",
        location = "component://humanres/script/org/ofbiz/humanres/HumanResServices.xml",
        invoke = "getCurrentPartyEmploymentData",
        description = "Delete Valid LeaveType",
        auth = "true",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "partyBenefitTypes", type = "java.util.List", mode = "OUT", optional = "true"),
            @Attribute(name = "employment", type = "org.ofbiz.entity.GenericValue", mode = "OUT", optional = "true"),
            @Attribute(name = "emplPosition", type = "org.ofbiz.entity.GenericValue", mode = "OUT", optional = "true"),
            @Attribute(name = "emplPositionType", type = "org.ofbiz.entity.GenericValue", mode = "OUT", optional = "true"),
            @Attribute(name = "emplPositionRateType", type = "org.ofbiz.entity.GenericValue", mode = "OUT", optional = "true"),
            @Attribute(name = "emplPositionRateAmount", type = "org.ofbiz.entity.GenericValue", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "VIEW")
    )
    public interface GetCurrentPartyEmploymentData {}

    /**
     * Create a new Job Requisition
     */
    @Service(
        name = "createJobRequisition",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a new Job Requisition",
        defaultEntityName = "JobRequisition",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "JobRequisition", mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(entityName = "JobRequisition", mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "noOfResources", optional = "false"),
            @OverrideAttribute(name = "qualification", optional = "false"),
            @OverrideAttribute(name = "jobLocation", optional = "false"),
            @OverrideAttribute(name = "skillTypeId", optional = "false"),
            @OverrideAttribute(name = "experienceMonths", optional = "false"),
            @OverrideAttribute(name = "experienceYears", optional = "false")
        }
    )
    public interface CreateJobRequisition {}

    /**
     * Update Job Requisition
     */
    @Service(
        name = "updateJobRequisition",
        engine = "entity-auto",
        invoke = "update",
        description = "Update Job Requisition",
        defaultEntityName = "JobRequisition",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "noOfResources", optional = "false"),
            @OverrideAttribute(name = "jobLocation", optional = "false"),
            @OverrideAttribute(name = "skillTypeId", optional = "false"),
            @OverrideAttribute(name = "experienceMonths", optional = "false"),
            @OverrideAttribute(name = "experienceYears", optional = "false")
        }
    )
    public interface UpdateJobRequisition {}

    /**
     * Delete a Job Requisition
     */
    @Service(
        name = "deleteJobRequisition",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a Job Requisition",
        defaultEntityName = "JobRequisition",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "DELETE")
    )
    public interface DeleteJobRequisition {}

    /**
     * Create a New Internal Job Posting
     */
    @Service(
        name = "createInternalJobPosting",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a New Internal Job Posting",
        defaultEntityName = "EmploymentApp",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "EmploymentApp", mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(entityName = "EmploymentApp", mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "applyingPartyId", optional = "false"),
            @OverrideAttribute(name = "approverPartyId", optional = "false"),
            @OverrideAttribute(name = "jobRequisitionId", optional = "false")
        }
    )
    public interface CreateInternalJobPosting {}

    /**
     * Update Internal Job Posting
     */
    @Service(
        name = "updateInternalJobPosting",
        engine = "entity-auto",
        invoke = "update",
        description = "Update Internal Job Posting",
        defaultEntityName = "EmploymentApp",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "applyingPartyId", optional = "false"),
            @OverrideAttribute(name = "approverPartyId", optional = "false"),
            @OverrideAttribute(name = "jobRequisitionId", optional = "false")
        }
    )
    public interface UpdateInternalJobPosting {}

    /**
     * Delete an Internal Job Posting
     */
    @Service(
        name = "deleteInternalJobPosting",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete an Internal Job Posting",
        defaultEntityName = "EmploymentApp",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "DELETE")
    )
    public interface DeleteInternalJobPosting {}

    /**
     * Create Job Interview
     */
    @Service(
        name = "createJobInterview",
        engine = "entity-auto",
        invoke = "create",
        description = "Create Job Interview",
        defaultEntityName = "JobInterview",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "JobInterview", mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(entityName = "JobInterview", mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "jobIntervieweePartyId", optional = "false"),
            @OverrideAttribute(name = "jobRequisitionId", optional = "false"),
            @OverrideAttribute(name = "jobInterviewerPartyId", optional = "false")
        }
    )
    public interface CreateJobInterview {}

    /**
     * Update Job Interview
     */
    @Service(
        name = "updateJobInterview",
        engine = "entity-auto",
        invoke = "update",
        description = "Update Job Interview",
        defaultEntityName = "JobInterview",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "jobIntervieweePartyId", optional = "false"),
            @OverrideAttribute(name = "jobRequisitionId", optional = "false"),
            @OverrideAttribute(name = "jobInterviewTypeId", optional = "false")
        }
    )
    public interface UpdateJobInterview {}

    /**
     * Delete Job Interview
     */
    @Service(
        name = "deleteJobInterview",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete Job Interview",
        defaultEntityName = "JobInterview",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "DELETE")
    )
    public interface DeleteJobInterview {}

    /**
     * Create a New Interview Type
     */
    @Service(
        name = "createJobInterviewType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a New Interview Type",
        defaultEntityName = "JobInterviewType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "description", optional = "false")
        }
    )
    public interface CreateJobInterviewType {}

    /**
     * Update Interview Type
     */
    @Service(
        name = "updateJobInterviewType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update Interview Type",
        defaultEntityName = "JobInterviewType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "description", optional = "false")
        }
    )
    public interface UpdateJobInterviewType {}

    /**
     * Delete Interview Type
     */
    @Service(
        name = "deleteJobInterviewType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete Interview Type",
        defaultEntityName = "JobInterviewType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "DELETE")
    )
    public interface DeleteJobInterviewType {}

    /**
     * Update Approval Status
     */
    @Service(
        name = "updateApprovalStatus",
        engine = "entity-auto",
        invoke = "update",
        description = "Update Approval Status",
        defaultEntityName = "EmploymentApp",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdateApprovalStatus {}

    /**
     * Update Training Status
     */
    @Service(
        name = "updateTrainingStatus",
        engine = "entity-auto",
        invoke = "update",
        description = "Update Training Status",
        defaultEntityName = "PersonTraining",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "reason", optional = "false")
        }
    )
    public interface UpdateTrainingStatus {}

    /**
     * Create Training Request
     */
    @Service(
        name = "applyTraining",
        engine = "simple",
        location = "component://humanres/script/org/ofbiz/humanres/HumanResServices.xml",
        invoke = "applyTraining",
        description = "Create Training Request",
        defaultEntityName = "PersonTraining",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "approverId", optional = "false"),
            @OverrideAttribute(name = "trainingClassTypeId", optional = "false"),
            @OverrideAttribute(name = "fromDate", optional = "true"),
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface ApplyTraining {}

    /**
     * Create Training Request
     */
    @Service(
        name = "assignTraining",
        engine = "simple",
        location = "component://humanres/script/org/ofbiz/humanres/HumanResServices.xml",
        invoke = "assignTraining",
        description = "Create Training Request",
        defaultEntityName = "PersonTraining",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "approverId", optional = "false"),
            @OverrideAttribute(name = "trainingClassTypeId", optional = "false")
        }
    )
    public interface AssignTraining {}

    /**
     * Create a New Training type
     */
    @Service(
        name = "createTrainingTypes",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a New Training type",
        defaultEntityName = "TrainingClassType",
        auth = "true",
        attributes = {
            @Attribute(name = "trainingClassTypeId", type = "String", mode = "IN"),
            @Attribute(name = "parentTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "description", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "CREATE")
    )
    public interface CreateTrainingTypes {}

    /**
     * Update a Training Type
     */
    @Service(
        name = "updateTrainingTypes",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Training Type",
        defaultEntityName = "TrainingClassType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "description", optional = "false")
        }
    )
    public interface UpdateTrainingTypes {}

    /**
     * Delete a Training Type
     */
    @Service(
        name = "deleteTrainingTypes",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a Training Type",
        defaultEntityName = "TrainingClassType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "DELETE")
    )
    public interface DeleteTrainingTypes {}

    /**
     * Create Valid Leave Reason Type
     */
    @Service(
        name = "createEmplLeaveReasonType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create Valid Leave Reason Type",
        defaultEntityName = "EmplLeaveReasonType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "description", optional = "false")
        }
    )
    public interface CreateEmplLeaveReasonType {}

    /**
     * Update Valid Leave Reason Type
     */
    @Service(
        name = "updateEmplLeaveReasonType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update Valid Leave Reason Type",
        defaultEntityName = "EmplLeaveReasonType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "description", optional = "false")
        }
    )
    public interface UpdateEmplLeaveReasonType {}

    /**
     * Delete Valid Leave Reason Type
     */
    @Service(
        name = "deleteEmplLeaveReasonType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete Valid Leave Reason Type",
        defaultEntityName = "EmplLeaveReasonType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "DELETE")
    )
    public interface DeleteEmplLeaveReasonType {}

    /**
     * Update Leave Approval Status
     */
    @Service(
        name = "updateEmplLeaveStatus",
        engine = "entity-auto",
        invoke = "update",
        description = "Update Leave Approval Status",
        defaultEntityName = "EmplLeave",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "humanResManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdateEmplLeaveStatus {}

    /**
     * Create a PartyQualType record
     */
    @Service(
        name = "createPartyQualType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a PartyQualType record",
        defaultEntityName = "PartyQualType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreatePartyQualType {}

    /**
     * Update a PartyQualType record
     */
    @Service(
        name = "updatePartyQualType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a PartyQualType record",
        defaultEntityName = "PartyQualType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdatePartyQualType {}

    /**
     * Delete a PartyQualType record
     */
    @Service(
        name = "deletePartyQualType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a PartyQualType record",
        defaultEntityName = "PartyQualType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeletePartyQualType {}

}

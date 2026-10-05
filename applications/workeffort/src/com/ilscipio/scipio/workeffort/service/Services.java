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
package com.ilscipio.scipio.workeffort.service;

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

    /**
     * WorkEffort Entity Interface
     */
    @Service(
        name = "interfaceWorkEffort",
        engine = "interface",
        description = "WorkEffort Entity Interface",
        defaultEntityName = "WorkEffort",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true", excludeFields = {"lastStatusUpdate", "revisionNumber", "createdDate", "createdByUserLogin", "lastModifiedDate", "lastModifiedByUserLogin"})
        },
        overrideAttributes = {
            @OverrideAttribute(name = "workEffortName", allowHtml = "any"),
            @OverrideAttribute(name = "description", allowHtml = "any")
        }
    )
    public interface InterfaceWorkEffort {}

    /**
     * Create a WorkEffort Entity
     */
    @Service(
        name = "createWorkEffort",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "createWorkEffort",
        description = "Create a WorkEffort Entity",
        defaultEntityName = "WorkEffort",
        implemented = {@Implements(service = "interfaceWorkEffort")},
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        },
        attributes = {
            @Attribute(name = "quickAssignPartyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "requirementId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "custRequestId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "communicationEventId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "webSiteId", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "workEffortGenericPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "workEffortTypeId", optional = "false"),
            @OverrideAttribute(name = "workEffortName", optional = "false", allowHtml = "any"),
            @OverrideAttribute(name = "currentStatusId", optional = "false"),
            @OverrideAttribute(name = "description", allowHtml = "any")
        }
    )
    public interface CreateWorkEffort {}

    /**
     * Create a WorkEffort Entity and assign to a party
     */
    @Service(
        name = "createWorkEffortAndPartyAssign",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "createWorkEffortAndPartyAssign",
        description = "Create a WorkEffort Entity and assign to a party",
        defaultEntityName = "WorkEffort",
        implemented = {@Implements(service = "interfaceWorkEffort")},
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        },
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "roleTypeId", type = "String", mode = "IN"),
            @Attribute(name = "statusId", type = "String", mode = "IN"),
            @Attribute(name = "quickAssignPartyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "requirementId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "communicationEventId", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "workEffortGenericPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "workEffortTypeId", optional = "false"),
            @OverrideAttribute(name = "workEffortName", optional = "false"),
            @OverrideAttribute(name = "currentStatusId", optional = "false")
        }
    )
    public interface CreateWorkEffortAndPartyAssign {}

    /**
     * Update a WorkEffort Entity
     */
    @Service(
        name = "updateWorkEffort",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "updateWorkEffort",
        description = "Update a WorkEffort Entity",
        defaultEntityName = "WorkEffort",
        implemented = {@Implements(service = "interfaceWorkEffort")},
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "communicationEventId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "reason", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "webSiteId", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "workEffortGenericPermission", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface UpdateWorkEffort {}

    /**
     * Delete a WorkEffort Entity
     */
    @Service(
        name = "deleteWorkEffort",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "deleteWorkEffort",
        description = "Delete a WorkEffort Entity",
        defaultEntityName = "WorkEffort",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface DeleteWorkEffort {}

    /**
     * Copies an existing WorkEffort to a new WorkEffort.
     */
    @Service(
        name = "copyWorkEffort",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "copyWorkEffort",
        description = "Copies an existing WorkEffort to a new WorkEffort.",
        auth = "true",
        transactionTimeout = "300",
        attributes = {
            @Attribute(name = "sourceWorkEffortId", type = "String", mode = "IN", description = "The ID of the WorkEffort to copy from."),
            @Attribute(name = "targetWorkEffortId", type = "String", mode = "IN", optional = "true", description = "The ID of the WorkEffort copy. If empty a new WorkEffort ID will be created."),
            @Attribute(name = "copyWorkEffortAssocs", type = "String", mode = "IN", optional = "true", description = "Copy WorkEffort associations (Y/N). Only child WorkEffort associations will be copied."),
            @Attribute(name = "deepCopy", type = "String", mode = "IN", optional = "true", description = "Copy associated WorkEfforts (Y/N). Used only when copyWorkEffortAssocs = Y."),
            @Attribute(name = "excludeExpiredAssocs", type = "String", mode = "IN", optional = "true", description = "Exclude expired associated WorkEfforts from copying (Y/N). Used only when copyWorkEffortAssocs = Y."),
            @Attribute(name = "copyRelatedValues", type = "String", mode = "IN", optional = "true", description = "Copy WorkEffort related values (Y/N)."),
            @Attribute(name = "excludeExpiredRelations", type = "String", mode = "IN", optional = "true", description = "Exclude expired WorkEffort related values from copying (Y/N). Used only when copyRelatedValues = Y."),
            @Attribute(name = "workEffortId", type = "String", mode = "OUT")
        },
        permissionService = @PermissionService(service = "workEffortGenericPermission", mainAction = "CREATE")
    )
    public interface CopyWorkEffort {}

    /**
     * Duplicate a Work Effort. If workEffortId is empty a new workEffortId will be generated.             Set the statusId of the new WorkEffort to this status, otherwise, set the status to the first of the             sequenceId of the statusTypeId
     */
    @Service(
        name = "duplicateWorkEffort",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "duplicateWorkEffort",
        description = "Duplicate a Work Effort. If workEffortId is empty a new workEffortId will be generated.\n            Set the statusId of the new WorkEffort to this status, otherwise, set the status to the first of the\n            sequenceId of the statusTypeId",
        auth = "true",
        attributes = {
            @Attribute(name = "workEffortId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "oldWorkEffortId", type = "String", mode = "IN"),
            @Attribute(name = "duplicateWorkEffortAssocs", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "duplicateWorkEffortContents", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "duplicateWorkEffortNotes", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "duplicateWorkEffortAssignmentRates", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "removeWorkEffortAssocs", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "removeWorkEffortContents", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "removeWorkEffortNotes", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "removeWorkEffortAssignmentRates", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "workEffortId", type = "String", mode = "OUT"),
            @Attribute(name = "statusId", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "workEffortGenericPermission", mainAction = "CREATE")
    )
    public interface DuplicateWorkEffort {}

    /**
     * Make a Communication Event Workeffort and create the workeffort itself if the ID not supplied
     */
    @Service(
        name = "makeCommunicationEventWorkEffort",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "makeCommunicationEventWorkEffort",
        description = "Make a Communication Event Workeffort and create the workeffort itself if the ID not supplied",
        defaultEntityName = "WorkEffort",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "communicationEventId", type = "String", mode = "INOUT"),
            @Attribute(name = "relationDescription", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface MakeCommunicationEventWorkEffort {}

    /**
     * Create a WorkEffortPartyAssignment Entity
     */
    @Service(
        name = "assignPartyToWorkEffort",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "assignPartyToWorkEffort",
        description = "Create a WorkEffortPartyAssignment Entity",
        defaultEntityName = "WorkEffortPartyAssignment",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true", excludeFields = {"statusDateTime"})
        },
        permissionService = @PermissionService(service = "workEffortGenericPermission", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", mode = "INOUT", optional = "true"),
            @OverrideAttribute(name = "partyId", optional = "false"),
            @OverrideAttribute(name = "roleTypeId", optional = "false"),
            @OverrideAttribute(name = "statusId", optional = "false"),
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface AssignPartyToWorkEffort {}

    /**
     * Update a WorkEffortPartyAssignment Entity
     */
    @Service(
        name = "updatePartyToWorkEffortAssignment",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "updatePartyToWorkEffortAssignment",
        description = "Update a WorkEffortPartyAssignment Entity",
        defaultEntityName = "WorkEffortPartyAssignment",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true", excludeFields = {"statusDateTime"})
        },
        permissionService = @PermissionService(service = "workEffortGenericPermission", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "false"),
            @OverrideAttribute(name = "partyId", optional = "false"),
            @OverrideAttribute(name = "roleTypeId", optional = "false"),
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface UpdatePartyToWorkEffortAssignment {}

    /**
     * delete/set the thrudate on the WorkEffortPartyAssignment Entity to today
     */
    @Service(
        name = "deletePartyToWorkEffortAssignment",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "deletePartyToWorkEffortAssignment",
        description = "delete/set the thrudate on the WorkEffortPartyAssignment Entity to today",
        defaultEntityName = "WorkEffortPartyAssignment",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "workEffortGenericPermission", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "false"),
            @OverrideAttribute(name = "partyId", optional = "false"),
            @OverrideAttribute(name = "roleTypeId", optional = "false"),
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface DeletePartyToWorkEffortAssignment {}

    /**
     * Delete a WorkEffortPartyAssignment Entity
     */
    @Service(
        name = "unassignPartyFromWorkEffort",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "unassignPartyFromWorkEffort",
        description = "Delete a WorkEffortPartyAssignment Entity",
        defaultEntityName = "WorkEffortPartyAssignment",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "workEffortGenericPermission", mainAction = "DELETE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "false"),
            @OverrideAttribute(name = "partyId", optional = "false"),
            @OverrideAttribute(name = "roleTypeId", optional = "false"),
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface UnassignPartyFromWorkEffort {}

    /**
     * Quick Assign Party To WorkEffort as Owner
     */
    @Service(
        name = "quickAssignPartyToWorkEffort",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "quickAssignPartyToWorkEffort",
        description = "Quick Assign Party To WorkEffort as Owner",
        attributes = {
            @Attribute(name = "workEffortId", type = "String", mode = "IN"),
            @Attribute(name = "quickAssignPartyId", type = "String", mode = "IN")
        }
    )
    public interface QuickAssignPartyToWorkEffort {}

    /**
     * Quick Assign Party To WorkEffort as Owner
     */
    @Service(
        name = "quickAssignPartyToWorkEffortWithRole",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "quickAssignPartyToWorkEffortWithRole",
        description = "Quick Assign Party To WorkEffort as Owner",
        attributes = {
            @Attribute(name = "workEffortId", type = "String", mode = "IN"),
            @Attribute(name = "quickAssignPartyId", type = "String", mode = "IN"),
            @Attribute(name = "roleTypeId", type = "String", mode = "IN")
        }
    )
    public interface QuickAssignPartyToWorkEffortWithRole {}

    /**
     * Create a WorkEffort Note
     */
    @Service(
        name = "createWorkEffortNote",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "createWorkEffortNote",
        description = "Create a WorkEffort Note",
        auth = "true",
        attributes = {
            @Attribute(name = "workEffortId", type = "String", mode = "IN"),
            @Attribute(name = "noteInfo", type = "String", mode = "IN"),
            @Attribute(name = "noteParty", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "noteName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "internalNote", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "noteId", type = "String", mode = "OUT")
        },
        permissionService = @PermissionService(service = "workEffortGenericPermission", mainAction = "UPDATE")
    )
    public interface CreateWorkEffortNote {}

    /**
     * Update a WorkEffort Note
     */
    @Service(
        name = "updateWorkEffortNote",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "updateWorkEffortNote",
        description = "Update a WorkEffort Note",
        auth = "true",
        attributes = {
            @Attribute(name = "workEffortId", type = "String", mode = "IN"),
            @Attribute(name = "noteId", type = "String", mode = "IN"),
            @Attribute(name = "internalNote", type = "String", mode = "IN"),
            @Attribute(name = "noteInfo", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface UpdateWorkEffortNote {}

    /**
     * Get the active WorkEffort Events where the logged in user is assigned in the specidied role.
     */
    @Service(
        name = "getWorkEffortAssignedEventsForRole",
        engine = "java",
        location = "org.ofbiz.workeffort.workeffort.WorkEffortServices",
        invoke = "getWorkEffortAssignedEventsForRole",
        description = "Get the active WorkEffort Events where the logged in user is assigned in the specidied role.",
        attributes = {
            @Attribute(name = "createdPeriod", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "roleTypeId", type = "String", mode = "IN"),
            @Attribute(name = "events", type = "java.util.List", mode = "OUT")
        }
    )
    public interface GetWorkEffortAssignedEventsForRole {}

    /**
     * Get the active WorkEffort Events in the specified role for all the parties.
     */
    @Service(
        name = "getWorkEffortAssignedEventsForRoleOfAllParties",
        engine = "java",
        location = "org.ofbiz.workeffort.workeffort.WorkEffortServices",
        invoke = "getWorkEffortAssignedEventsForRoleOfAllParties",
        description = "Get the active WorkEffort Events in the specified role for all the parties.",
        attributes = {
            @Attribute(name = "createdPeriod", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "roleTypeId", type = "String", mode = "IN"),
            @Attribute(name = "events", type = "java.util.List", mode = "OUT")
        }
    )
    public interface GetWorkEffortAssignedEventsForRoleOfAllParties {}

    /**
     * Get WorkEffort Assigned Tasks : workEffort assign to userLogin.partyId and with             workEffortTypeId = TASK and currentStatusId not in (CAL_DECLINED, CAL_DELEGATED, CAL_COMPLETED, CAL_CANCELLED) and partyAssign.statusId != PRTYASGN_UNASSIGNED             OR  workEffortTypeId = PROD_ORDER_TASK and currentStatusId not in (PRUN_CANCELLED, PRUN_COMPLETED, PRUN_CLOSED)
     */
    @Service(
        name = "getWorkEffortAssignedTasks",
        engine = "java",
        location = "org.ofbiz.workeffort.workeffort.WorkEffortServices",
        invoke = "getWorkEffortAssignedTasks",
        description = "Get WorkEffort Assigned Tasks : workEffort assign to userLogin.partyId and with\n            workEffortTypeId = TASK and currentStatusId not in (CAL_DECLINED, CAL_DELEGATED, CAL_COMPLETED, CAL_CANCELLED) and partyAssign.statusId != PRTYASGN_UNASSIGNED\n            OR  workEffortTypeId = PROD_ORDER_TASK and currentStatusId not in (PRUN_CANCELLED, PRUN_COMPLETED, PRUN_CLOSED)",
        attributes = {
            @Attribute(name = "createdPeriod", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "tasks", type = "java.util.List", mode = "OUT")
        }
    )
    public interface GetWorkEffortAssignedTasks {}

    /**
     * Get WorkEffort Assigned Activities : workEffort assign to userLogin.partyId and with             workEffortTypeId = ACTIVITY and currentStatusId not in (WF_COMPLETED, WF_TERMINATED, WF_ABORTED)             and partyAssign.statusId not in (CAL_DECLINED, CAL_DELEGATED, CAL_COMPLETED, CAL_CANCELLED, PRTYASGN_UNASSIGNED)
     */
    @Service(
        name = "getWorkEffortAssignedActivities",
        engine = "java",
        location = "org.ofbiz.workeffort.workeffort.WorkEffortServices",
        invoke = "getWorkEffortAssignedActivities",
        description = "Get WorkEffort Assigned Activities : workEffort assign to userLogin.partyId and with\n            workEffortTypeId = ACTIVITY and currentStatusId not in (WF_COMPLETED, WF_TERMINATED, WF_ABORTED)\n            and partyAssign.statusId not in (CAL_DECLINED, CAL_DELEGATED, CAL_COMPLETED, CAL_CANCELLED, PRTYASGN_UNASSIGNED)",
        attributes = {
            @Attribute(name = "createdPeriod", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "activities", type = "java.util.List", mode = "OUT")
        }
    )
    public interface GetWorkEffortAssignedActivities {}

    /**
     * Get WorkEffort Assigned Activities By Role : same condition as getWorkEffortAssignedActivities but on view WorkEffortPartyAssignByRole             to be able to have all party roles
     */
    @Service(
        name = "getWorkEffortAssignedActivitiesByRole",
        engine = "java",
        location = "org.ofbiz.workeffort.workeffort.WorkEffortServices",
        invoke = "getWorkEffortAssignedActivitiesByRole",
        description = "Get WorkEffort Assigned Activities By Role : same condition as getWorkEffortAssignedActivities but on view WorkEffortPartyAssignByRole\n            to be able to have all party roles",
        attributes = {
            @Attribute(name = "createdPeriod", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "roleActivities", type = "java.util.List", mode = "OUT")
        }
    )
    public interface GetWorkEffortAssignedActivitiesByRole {}

    /**
     * Get WorkEffort Assigned Activities By Group : same condition as getWorkEffortAssignedActivities but on view WorkEffortPartyAssignByGroup             to be able to have all parties associated to userLogin.partyId by PartyRelationship
     */
    @Service(
        name = "getWorkEffortAssignedActivitiesByGroup",
        engine = "java",
        location = "org.ofbiz.workeffort.workeffort.WorkEffortServices",
        invoke = "getWorkEffortAssignedActivitiesByGroup",
        description = "Get WorkEffort Assigned Activities By Group : same condition as getWorkEffortAssignedActivities but on view WorkEffortPartyAssignByGroup\n            to be able to have all parties associated to userLogin.partyId by PartyRelationship",
        attributes = {
            @Attribute(name = "createdPeriod", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "groupActivities", type = "java.util.List", mode = "OUT")
        }
    )
    public interface GetWorkEffortAssignedActivitiesByGroup {}

    /**
     * Get WorkEffort Completed Tasks : workEffort assign to userLogin.partyId and with             workEffortTypeId = TASK and currentStatusId in (CAL_DECLINED, CAL_DELEGATED, CAL_COMPLETED, CAL_CANCELLED)             OR  workEffortTypeId = PROD_ORDER_TASK and currentStatusId in (PRUN_CANCELLED, PRUN_COMPLETED, PRUN_CLOSED)
     */
    @Service(
        name = "getWorkEffortCompletedTasks",
        engine = "java",
        location = "org.ofbiz.workeffort.workeffort.WorkEffortServices",
        invoke = "getWorkEffortCompletedTasks",
        description = "Get WorkEffort Completed Tasks : workEffort assign to userLogin.partyId and with\n            workEffortTypeId = TASK and currentStatusId in (CAL_DECLINED, CAL_DELEGATED, CAL_COMPLETED, CAL_CANCELLED)\n            OR  workEffortTypeId = PROD_ORDER_TASK and currentStatusId in (PRUN_CANCELLED, PRUN_COMPLETED, PRUN_CLOSED)",
        attributes = {
            @Attribute(name = "createdPeriod", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "tasks", type = "java.util.List", mode = "OUT")
        }
    )
    public interface GetWorkEffortCompletedTasks {}

    /**
     * Get WorkEffort Completed Activities : workEffort assign to userLogin.partyId and with             workEffortTypeId = ACTIVITY and partyAssign.statusId in (WF_COMPLETED, WF_TERMINATED, WF_ABORTED)             and currentStatusId in (CAL_DECLINED, CAL_DELEGATED, CAL_COMPLETED, CAL_CANCELLED, PRTYASGN_UNASSIGNED)
     */
    @Service(
        name = "getWorkEffortCompletedActivities",
        engine = "java",
        location = "org.ofbiz.workeffort.workeffort.WorkEffortServices",
        invoke = "getWorkEffortCompletedActivities",
        description = "Get WorkEffort Completed Activities : workEffort assign to userLogin.partyId and with\n            workEffortTypeId = ACTIVITY and partyAssign.statusId in (WF_COMPLETED, WF_TERMINATED, WF_ABORTED)\n            and currentStatusId in (CAL_DECLINED, CAL_DELEGATED, CAL_COMPLETED, CAL_CANCELLED, PRTYASGN_UNASSIGNED)",
        attributes = {
            @Attribute(name = "createdPeriod", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "activities", type = "java.util.List", mode = "OUT")
        }
    )
    public interface GetWorkEffortCompletedActivities {}

    /**
     * Get WorkEffort
     */
    @Service(
        name = "getWorkEffort",
        engine = "java",
        location = "org.ofbiz.workeffort.workeffort.WorkEffortServices",
        invoke = "getWorkEffort",
        description = "Get WorkEffort",
        attributes = {
            @Attribute(name = "workEffortId", type = "java.lang.String", mode = "INOUT", optional = "true"),
            @Attribute(name = "currentStatusId", type = "java.lang.String", mode = "IN", optional = "true"),
            @Attribute(name = "workEffort", type = "org.ofbiz.entity.GenericValue", mode = "OUT", optional = "true"),
            @Attribute(name = "canView", type = "java.lang.Boolean", mode = "OUT", optional = "true"),
            @Attribute(name = "tryEntity", type = "java.lang.Boolean", mode = "OUT", optional = "true"),
            @Attribute(name = "currentStatusItem", type = "org.ofbiz.entity.GenericValue", mode = "OUT", optional = "true"),
            @Attribute(name = "partyAssigns", type = "java.util.Collection", mode = "OUT", optional = "true")
        }
    )
    public interface GetWorkEffort {}

    /**
     * Get WorkEffort Events by a period specified by periodType attribute (one of the           java.util.Calendar field values). Return a Map with periodStart as the key and a Collection of events for that period as value           If filterOutCanceledEvents is set to Boolean(true) then workEfforts with currentStatusId=EVENT_CANCELLED will not be returned.           To limit the events to a particular partyId, specify the partyId.  To limit the events to a set of partyIds, specify a Collection of partyIds.         
     */
    @Service(
        name = "getWorkEffortEventsByPeriod",
        engine = "java",
        location = "org.ofbiz.workeffort.workeffort.WorkEffortServices",
        invoke = "getWorkEffortEventsByPeriod",
        description = "Get WorkEffort Events by a period specified by periodType attribute (one of the\n          java.util.Calendar field values). Return a Map with periodStart as the key and a Collection of events for that period as value\n          If filterOutCanceledEvents is set to Boolean(true) then workEfforts with currentStatusId=EVENT_CANCELLED will not be returned.\n          To limit the events to a particular partyId, specify the partyId.  To limit the events to a set of partyIds, specify a Collection of partyIds.\n        ",
        auth = "true",
        attributes = {
            @Attribute(name = "calendarType", type = "String", mode = "IN", optional = "true", description = "Can be either CAL_PERSONAL or CAL_MANUFACTURING, defaults to CAL_PERSONAL. The value controls the type of work efforts\n            returned. To bypass this behavior, use an invalid value (like VOID)."),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyIds", type = "java.util.Collection", mode = "IN", optional = "true"),
            @Attribute(name = "facilityId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fixedAssetId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "workEffortTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "start", type = "java.sql.Timestamp", mode = "IN"),
            @Attribute(name = "numPeriods", type = "java.lang.Integer", mode = "IN"),
            @Attribute(name = "periodType", type = "java.lang.Integer", mode = "IN"),
            @Attribute(name = "filterOutCanceledEvents", type = "java.lang.Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "entityExprList", type = "java.util.List", mode = "IN", optional = "true"),
            @Attribute(name = "periods", type = "java.util.List", mode = "OUT"),
            @Attribute(name = "maxConcurrentEntries", type = "java.lang.Integer", mode = "OUT")
        }
    )
    public interface GetWorkEffortEventsByPeriod {}

    /**
     * Removes duplicate work efforts from a list of work efforts.
     */
    @Service(
        name = "removeDuplicateWorkEfforts",
        engine = "java",
        location = "org.ofbiz.workeffort.workeffort.WorkEffortServices",
        invoke = "removeDuplicateWorkEfforts",
        description = "Removes duplicate work efforts from a list of work efforts.",
        auth = "true",
        attributes = {
            @Attribute(name = "workEffortIterator", type = "java.util.ListIterator", mode = "IN", optional = "true"),
            @Attribute(name = "workEfforts", type = "java.util.List", mode = "INOUT", optional = "true")
        }
    )
    public interface RemoveDuplicateWorkEfforts {}

    /**
     *              Create a map with an entry for each facility. The value is another map with information about             the manufacturing orders running in the facility for the given product:             incoming - incomingProductionRunList, estimatedQuantityTotal.  Shows quantity of product to be produced.             outgoing - outgoingProductionRunList, estimatedQuantityTotal.  Shows quantity of product to be consumed.         
     */
    @Service(
        name = "getProductManufacturingSummaryByFacility",
        engine = "java",
        location = "org.ofbiz.workeffort.workeffort.WorkEffortServices",
        invoke = "getProductManufacturingSummaryByFacility",
        description = "\n            Create a map with an entry for each facility. The value is another map with information about\n            the manufacturing orders running in the facility for the given product:\n            incoming - incomingProductionRunList, estimatedQuantityTotal.  Shows quantity of product to be produced.\n            outgoing - outgoingProductionRunList, estimatedQuantityTotal.  Shows quantity of product to be consumed.\n        ",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "facilityId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "summaryInByFacility", type = "Map", mode = "OUT"),
            @Attribute(name = "summaryOutByFacility", type = "Map", mode = "OUT")
        }
    )
    public interface GetProductManufacturingSummaryByFacility {}

    /**
     *              Create a WorkEffort Assoc, for linking task to describe a project or             for linking routing with its routingTasks         
     */
    @Service(
        name = "createWorkEffortAssoc",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "createWorkEffortAssoc",
        description = "\n            Create a WorkEffort Assoc, for linking task to describe a project or\n            for linking routing with its routingTasks\n        ",
        defaultEntityName = "WorkEffortAssoc",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "workEffortGenericPermission", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "workEffortAssocTypeId", optional = "false"),
            @OverrideAttribute(name = "workEffortIdFrom", optional = "false"),
            @OverrideAttribute(name = "workEffortIdTo", optional = "false"),
            @OverrideAttribute(name = "fromDate", mode = "INOUT", optional = "true")
        }
    )
    public interface CreateWorkEffortAssoc {}

    /**
     *              Update a WorkEffort Assoc, for linking task to describe a project or             for linking routing with its routingTasks         
     */
    @Service(
        name = "updateWorkEffortAssoc",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "updateWorkEffortAssoc",
        description = "\n            Update a WorkEffort Assoc, for linking task to describe a project or\n            for linking routing with its routingTasks\n        ",
        defaultEntityName = "WorkEffortAssoc",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "workEffortGenericPermission", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "false"),
            @OverrideAttribute(name = "workEffortAssocTypeId", optional = "false"),
            @OverrideAttribute(name = "workEffortIdFrom", optional = "false"),
            @OverrideAttribute(name = "workEffortIdTo", optional = "false")
        }
    )
    public interface UpdateWorkEffortAssoc {}

    /**
     *              Remove a WorkEffort Assoc, for linking task to describe a project or             for linking routing with its routingTasks         
     */
    @Service(
        name = "removeWorkEffortAssoc",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "removeWorkEffortAssoc",
        description = "\n            Remove a WorkEffort Assoc, for linking task to describe a project or\n            for linking routing with its routingTasks\n        ",
        defaultEntityName = "WorkEffortAssoc",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "workEffortGenericPermission", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "false"),
            @OverrideAttribute(name = "workEffortAssocTypeId", optional = "false"),
            @OverrideAttribute(name = "workEffortIdFrom", optional = "false"),
            @OverrideAttribute(name = "workEffortIdTo", optional = "false")
        }
    )
    public interface RemoveWorkEffortAssoc {}

    /**
     * Copies WorkEffortAssocs from one WorkEffort to another WorkEffort. Only child WorkEffort associations will be copied.
     */
    @Service(
        name = "copyWorkEffortAssocs",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "copyWorkEffortAssocs",
        description = "Copies WorkEffortAssocs from one WorkEffort to another WorkEffort. Only child WorkEffort associations will be copied.",
        auth = "true",
        transactionTimeout = "300",
        attributes = {
            @Attribute(name = "sourceWorkEffortId", type = "String", mode = "IN", description = "The ID of the WorkEffort to copy the associations from."),
            @Attribute(name = "targetWorkEffortId", type = "String", mode = "IN", optional = "true", description = "The ID of the WorkEffort to copy the associations to."),
            @Attribute(name = "deepCopy", type = "String", mode = "IN", optional = "true", description = "Copy associated WorkEfforts (Y/N)."),
            @Attribute(name = "excludeExpiredAssocs", type = "String", mode = "IN", optional = "true", description = "Exclude expired WorkEffort associations from copying (Y/N)."),
            @Attribute(name = "copyRelatedValues", type = "String", mode = "IN", optional = "true", description = "Copy WorkEffort related values (Y/N). Used only when deepCopy = Y."),
            @Attribute(name = "excludeExpiredRelations", type = "String", mode = "IN", optional = "true", description = "Exclude expired WorkEffort related values from copying (Y/N). Used only when deepCopy = Y.")
        },
        permissionService = @PermissionService(service = "workEffortGenericPermission", mainAction = "CREATE")
    )
    public interface CopyWorkEffortAssocs {}

    /**
     * Creates a WorkEffort entity and WorkEffortAssoc
     */
    @Service(
        name = "createWorkEffortAndAssoc",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "createWorkEffortAndAssoc",
        description = "Creates a WorkEffort entity and WorkEffortAssoc",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "WorkEffort", mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(entityName = "WorkEffort", mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(entityName = "WorkEffortAssoc", mode = "INOUT", include = "pk"),
            @EntityAttributes(entityName = "WorkEffortAssoc", mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "quickAssignPartyId", type = "String", mode = "IN", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "workEffortIdTo", optional = "true"),
            @OverrideAttribute(name = "fromDate", optional = "true"),
            @OverrideAttribute(name = "currentStatusId", optional = "false"),
            @OverrideAttribute(name = "workEffortName", optional = "false"),
            @OverrideAttribute(name = "workEffortAssocTypeId", optional = "false"),
            @OverrideAttribute(name = "workEffortIdFrom", optional = "false"),
            @OverrideAttribute(name = "workEffortTypeId", optional = "false")
        }
    )
    public interface CreateWorkEffortAndAssoc {}

    /**
     * Creates a WorkEffort entity and WorkEffortAssoc
     */
    @Service(
        name = "updateWorkEffortAndAssoc",
        engine = "group",
        location = "updateWorkEffortAndAssoc",
        description = "Creates a WorkEffort entity and WorkEffortAssoc",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "WorkEffort", mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(entityName = "WorkEffort", mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(entityName = "WorkEffortAssoc", mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(entityName = "WorkEffortAssoc", mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateWorkEffortAndAssoc {}

    /**
     *              Create a WorkEffort - Product Assoc, for linking WorkEffort to In or Out  Product,             for routing it's the link between Manufactured Product with its routings         
     */
    @Service(
        name = "createWorkEffortGoodStandard",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "createWorkEffortGoodStandard",
        description = "\n            Create a WorkEffort - Product Assoc, for linking WorkEffort to In or Out  Product,\n            for routing it's the link between Manufactured Product with its routings\n        ",
        defaultEntityName = "WorkEffortGoodStandard",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "workEffortGenericPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "true"),
            @OverrideAttribute(name = "productId", optional = "false"),
            @OverrideAttribute(name = "workEffortGoodStdTypeId", optional = "false"),
            @OverrideAttribute(name = "workEffortId", optional = "false"),
            @OverrideAttribute(name = "estimatedQuantity", typeConvert = "true")
        }
    )
    public interface CreateWorkEffortGoodStandard {}

    /**
     *              Update a WorkEffort - Product Assoc, for linking WorkEffort to In or Out  Product,             for routing it's the link between Manufactured Product with its routings         
     */
    @Service(
        name = "updateWorkEffortGoodStandard",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "updateWorkEffortGoodStandard",
        description = "\n            Update a WorkEffort - Product Assoc, for linking WorkEffort to In or Out  Product,\n            for routing it's the link between Manufactured Product with its routings\n        ",
        defaultEntityName = "WorkEffortGoodStandard",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "workEffortGenericPermission", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "false"),
            @OverrideAttribute(name = "productId", optional = "false"),
            @OverrideAttribute(name = "workEffortGoodStdTypeId", optional = "false"),
            @OverrideAttribute(name = "workEffortId", optional = "false"),
            @OverrideAttribute(name = "estimatedQuantity", typeConvert = "true")
        }
    )
    public interface UpdateWorkEffortGoodStandard {}

    /**
     * Remove a WorkEffort - Product Assoc, for linking WorkEffort to In or Out  Product,             for routing it's the link between Manufactured Product with its routings         
     */
    @Service(
        name = "removeWorkEffortGoodStandard",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "removeWorkEffortGoodStandard",
        description = "Remove a WorkEffort - Product Assoc, for linking WorkEffort to In or Out  Product,\n            for routing it's the link between Manufactured Product with its routings\n        ",
        defaultEntityName = "WorkEffortGoodStandard",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "false"),
            @OverrideAttribute(name = "productId", optional = "false"),
            @OverrideAttribute(name = "workEffortGoodStdTypeId", optional = "false"),
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface RemoveWorkEffortGoodStandard {}

    /**
     * Create or update WorkEffortInventoryAssign
     */
    @Service(
        name = "assignInventoryToWorkEffort",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "assignInventoryToWorkEffort",
        description = "Create or update WorkEffortInventoryAssign",
        defaultEntityName = "WorkEffortInventoryAssign",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "workEffortGenericPermission", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "inventoryItemId", optional = "false"),
            @OverrideAttribute(name = "workEffortId", optional = "false"),
            @OverrideAttribute(name = "quantity", typeConvert = "true")
        }
    )
    public interface AssignInventoryToWorkEffort {}

    /**
     * Creates a CommunicationEvent entity and CommunicationEventWorkEff
     */
    @Service(
        name = "createCommunicationEventWorkEff",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "makeCommunicationEventWorkEffort",
        description = "Creates a CommunicationEvent entity and CommunicationEventWorkEff",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "CommunicationEvent", mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(entityName = "CommunicationEvent", mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(entityName = "CommunicationEventWorkEff", mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(entityName = "CommunicationEventWorkEff", mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateCommunicationEventWorkEff {}

    /**
     * Updates CommunicationEventWorkEff
     */
    @Service(
        name = "updateCommunicationEventWorkEff",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "updateCommunicationEventWorkEff",
        description = "Updates CommunicationEventWorkEff",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "CommunicationEventWorkEff", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "CommunicationEventWorkEff", mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "workEffortManagerPermission", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "communicationEventId", optional = "false"),
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface UpdateCommunicationEventWorkEff {}

    /**
     * Deletes CommunicationEventWorkEff
     */
    @Service(
        name = "deleteCommunicationEventWorkEff",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "deleteCommunicationEventWorkEff",
        description = "Deletes CommunicationEventWorkEff",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "CommunicationEventWorkEff", mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "workEffortManagerPermission", mainAction = "DELETE"),
        overrideAttributes = {
            @OverrideAttribute(name = "communicationEventId", optional = "false"),
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface DeleteCommunicationEventWorkEff {}

    /**
     * Creates a CustRequestWorkEffort
     */
    @Service(
        name = "createWorkEffortRequest",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "createWorkEffortRequest",
        description = "Creates a CustRequestWorkEffort",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "CustRequestWorkEffort", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "CustRequest", mode = "IN", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "custRequestId", mode = "INOUT", optional = "true"),
            @OverrideAttribute(name = "workEffortId", optional = "false"),
            @OverrideAttribute(name = "description", allowHtml = "any")
        }
    )
    public interface CreateWorkEffortRequest {}

    /**
     * Deletes a CustRequestWorkEffort
     */
    @Service(
        name = "deleteWorkEffortRequest",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "deleteWorkEffortRequest",
        description = "Deletes a CustRequestWorkEffort",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "CustRequestWorkEffort", mode = "IN", include = "pk")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "custRequestId", optional = "false"),
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface DeleteWorkEffortRequest {}

    /**
     * Creates a CustRequestItem entity and CustRequestItemWorkEffort
     */
    @Service(
        name = "createWorkEffortRequestItemAndRequestItem",
        engine = "group",
        location = "createWorkEffortRequestItemAndRequestItem",
        description = "Creates a CustRequestItem entity and CustRequestItemWorkEffort",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "CustRequestItemWorkEffort", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "CustRequestItem", mode = "IN", optional = "true")
        },
        attributes = {
            @Attribute(name = "custRequestItemExists", type = "java.lang.String", mode = "OUT", optional = "true")
        }
    )
    public interface CreateWorkEffortRequestItemAndRequestItem {}

    /**
     * Creates a CustRequestItemWorkEffort
     */
    @Service(
        name = "createWorkEffortRequestItem",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "createWorkEffortRequestItem",
        description = "Creates a CustRequestItemWorkEffort",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "CustRequestItemWorkEffort", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "CustRequestItem", mode = "IN", optional = "true")
        },
        attributes = {
            @Attribute(name = "custRequestItemExists", type = "java.lang.String", mode = "IN", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "custRequestId", optional = "false"),
            @OverrideAttribute(name = "custRequestItemSeqId", optional = "false"),
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface CreateWorkEffortRequestItem {}

    /**
     * Deletes a CustRequestItemWorkEffort
     */
    @Service(
        name = "deleteWorkEffortRequestItem",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "deleteWorkEffortRequestItem",
        description = "Deletes a CustRequestItemWorkEffort",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "CustRequestItemWorkEffort", mode = "IN", include = "pk")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "custRequestId", optional = "false"),
            @OverrideAttribute(name = "custRequestItemSeqId", optional = "false"),
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface DeleteWorkEffortRequestItem {}

    /**
     * Checks to see if a CustRequestItem exists
     */
    @Service(
        name = "checkCustRequestItemExists",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "checkCustRequestItemExists",
        description = "Checks to see if a CustRequestItem exists",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "CustRequestItem", mode = "IN", include = "pk", optional = "true")
        },
        attributes = {
            @Attribute(name = "custRequestItemExists", type = "java.lang.String", mode = "OUT", optional = "true")
        }
    )
    public interface CheckCustRequestItemExists {}

    /**
     * Creates a QuoteWorkEffort
     */
    @Service(
        name = "createWorkEffortQuote",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "createWorkEffortQuote",
        description = "Creates a QuoteWorkEffort",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "QuoteWorkEffort", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "Quote", mode = "IN", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "quoteId", mode = "INOUT", optional = "false"),
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface CreateWorkEffortQuote {}

    /**
     * Deletes a QuoteWorkEffort
     */
    @Service(
        name = "deleteWorkEffortQuote",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "deleteWorkEffortQuote",
        description = "Deletes a QuoteWorkEffort",
        defaultEntityName = "QuoteWorkEffort",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "quoteId", optional = "false"),
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface DeleteWorkEffortQuote {}

    /**
     * Creates a WorkRequirementFulfillment
     */
    @Service(
        name = "createWorkRequirementFulfillment",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "createWorkRequirementFulfillment",
        description = "Creates a WorkRequirementFulfillment",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "WorkRequirementFulfillment", mode = "IN", include = "pk", optional = "true"),
            @EntityAttributes(entityName = "WorkRequirementFulfillment", mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(entityName = "Requirement", mode = "IN", optional = "true")
        }
    )
    public interface CreateWorkRequirementFulfillment {}

    /**
     * Deletes a WorkRequirementFulfillment
     */
    @Service(
        name = "deleteWorkRequirementFulfillment",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "deleteWorkRequirementFulfillment",
        description = "Deletes a WorkRequirementFulfillment",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "WorkRequirementFulfillment", mode = "IN", include = "pk")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "workEffortId", optional = "false"),
            @OverrideAttribute(name = "requirementId", optional = "false")
        }
    )
    public interface DeleteWorkRequirementFulfillment {}

    /**
     * Creates a ShoppingListWorkEffort
     */
    @Service(
        name = "createShoppingListWorkEffort",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "createShoppingListWorkEffort",
        description = "Creates a ShoppingListWorkEffort",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "ShoppingListWorkEffort", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "ShoppingListWorkEffort", mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(entityName = "ShoppingList", mode = "IN", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "shoppingListId", mode = "INOUT", optional = "true"),
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface CreateShoppingListWorkEffort {}

    /**
     * Deletes a ShoppingListWorkEffort
     */
    @Service(
        name = "deleteShoppingListWorkEffort",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "deleteShoppingListWorkEffort",
        description = "Deletes a ShoppingListWorkEffort",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "ShoppingListWorkEffort", mode = "IN", include = "pk")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "shoppingListId", optional = "false"),
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface DeleteShoppingListWorkEffort {}

    /**
     * Creates a OrderHeaderWorkEffort
     */
    @Service(
        name = "createOrderHeaderWorkEffort",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "createOrderHeaderWorkEffort",
        description = "Creates a OrderHeaderWorkEffort",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "OrderHeaderWorkEffort", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "OrderHeaderWorkEffort", mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(entityName = "OrderHeader", mode = "IN", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "orderId", mode = "INOUT", optional = "true"),
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface CreateOrderHeaderWorkEffort {}

    /**
     * Deletes a OrderHeaderWorkEffort
     */
    @Service(
        name = "deleteOrderHeaderWorkEffort",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "deleteOrderHeaderWorkEffort",
        description = "Deletes a OrderHeaderWorkEffort",
        defaultEntityName = "OrderHeaderWorkEffort",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "orderId", optional = "false"),
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface DeleteOrderHeaderWorkEffort {}

    /**
     * Based on task's estimate dates, write assign entries for the fixed asset the task is assigned to
     */
    @Service(
        name = "setWorkEffortFixedAssetAssign",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "setWorkEffortFixedAssetAssign",
        description = "Based on task's estimate dates, write assign entries for the fixed asset the task is assigned to",
        auth = "true",
        attributes = {
            @Attribute(name = "workEffortId", type = "String", mode = "IN")
        }
    )
    public interface SetWorkEffortFixedAssetAssign {}

    /**
     * Creates a WorkEffortFixedAssetStd entry to associate a routing task             with a fixed asset (type)
     */
    @Service(
        name = "createWorkEffortFixedAssetStd",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "createWorkEffortFixedAssetStd",
        description = "Creates a WorkEffortFixedAssetStd entry to associate a routing task\n            with a fixed asset (type)",
        defaultEntityName = "WorkEffortFixedAssetStd",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "workEffortManagerPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fixedAssetTypeId", optional = "false"),
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface CreateWorkEffortFixedAssetStd {}

    /**
     * Updates an existing WorkEffortFixedAssetStd entry
     */
    @Service(
        name = "updateWorkEffortFixedAssetStd",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "updateWorkEffortFixedAssetStd",
        description = "Updates an existing WorkEffortFixedAssetStd entry",
        defaultEntityName = "WorkEffortFixedAssetStd",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "workEffortManagerPermission", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fixedAssetTypeId", optional = "false"),
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface UpdateWorkEffortFixedAssetStd {}

    /**
     * Removes a WorkEffortFixedAssetStd, thus removing the association between a routing task             and a fixed asset (type)
     */
    @Service(
        name = "removeWorkEffortFixedAssetStd",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "removeWorkEffortFixedAssetStd",
        description = "Removes a WorkEffortFixedAssetStd, thus removing the association between a routing task\n            and a fixed asset (type)",
        defaultEntityName = "WorkEffortFixedAssetStd",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "workEffortManagerPermission", mainAction = "DELETE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fixedAssetTypeId", optional = "false"),
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface RemoveWorkEffortFixedAssetStd {}

    /**
     * Create a WorkEffortFixedAssetAssign entry to associate a fixed asset             with a work effort (e.g. a production run task)
     */
    @Service(
        name = "createWorkEffortFixedAssetAssign",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "createWorkEffortFixedAssetAssign",
        description = "Create a WorkEffortFixedAssetAssign entry to associate a fixed asset\n            with a work effort (e.g. a production run task)",
        defaultEntityName = "WorkEffortFixedAssetAssign",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "workEffortManagerPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", mode = "IN", optional = "true"),
            @OverrideAttribute(name = "fixedAssetId", optional = "false"),
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface CreateWorkEffortFixedAssetAssign {}

    /**
     * Update an existing WorkEffortFixedAssetAssign entry
     */
    @Service(
        name = "updateWorkEffortFixedAssetAssign",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "updateWorkEffortFixedAssetAssign",
        description = "Update an existing WorkEffortFixedAssetAssign entry",
        defaultEntityName = "WorkEffortFixedAssetAssign",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "workEffortManagerPermission", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fixedAssetId", optional = "false"),
            @OverrideAttribute(name = "fromDate", optional = "false"),
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface UpdateWorkEffortFixedAssetAssign {}

    /**
     * Remove a WorkEffortFixedAssign entry, which removes the association between a fixed asset             and a work effort (e.g. a production run task)
     */
    @Service(
        name = "removeWorkEffortFixedAssetAssign",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "removeWorkEffortFixedAssetAssign",
        description = "Remove a WorkEffortFixedAssign entry, which removes the association between a fixed asset\n            and a work effort (e.g. a production run task)",
        defaultEntityName = "WorkEffortFixedAssetAssign",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "workEffortManagerPermission", mainAction = "DELETE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fixedAssetId", optional = "false"),
            @OverrideAttribute(name = "fromDate", optional = "false"),
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface RemoveWorkEffortFixedAssetAssign {}

    /**
     * Create a Work Effort Content
     */
    @Service(
        name = "createWorkEffortContent",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "createWorkEffortContent",
        description = "Create a Work Effort Content",
        defaultEntityName = "WorkEffortContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "workEffortGenericPermission", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "true"),
            @OverrideAttribute(name = "contentId", optional = "false"),
            @OverrideAttribute(name = "workEffortContentTypeId", optional = "false"),
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface CreateWorkEffortContent {}

    /**
     * Update a Work Effort Content
     */
    @Service(
        name = "updateWorkEffortContent",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "updateWorkEffortContent",
        description = "Update a Work Effort Content",
        defaultEntityName = "WorkEffortContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "workEffortGenericPermission", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "contentId", optional = "false"),
            @OverrideAttribute(name = "fromDate", optional = "false"),
            @OverrideAttribute(name = "workEffortContentTypeId", optional = "false"),
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface UpdateWorkEffortContent {}

    /**
     * Delete a Work Effort Content
     */
    @Service(
        name = "deleteWorkEffortContent",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "deleteWorkEffortContent",
        description = "Delete a Work Effort Content",
        defaultEntityName = "WorkEffortContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "contentId", optional = "false"),
            @OverrideAttribute(name = "fromDate", optional = "false"),
            @OverrideAttribute(name = "workEffortContentTypeId", optional = "false"),
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface DeleteWorkEffortContent {}

    /**
     * Creates a Text Document DataResource and Content Records
     */
    @Service(
        name = "createWorkEffortTextContent",
        engine = "group",
        description = "Creates a Text Document DataResource and Content Records",
        auth = "true",
        transactionTimeout = "300",
        invokes = {@GroupInvoke(name = "createTextContent", resultToContext = "true"), @GroupInvoke(name = "createWorkEffortContent", resultToContext = "false")}
    )
    public interface CreateWorkEffortTextContent {}

    /**
     * Update a Text Document DataResource and Content Records
     */
    @Service(
        name = "updateWorkEffortTextContent",
        engine = "group",
        description = "Update a Text Document DataResource and Content Records",
        auth = "true",
        transactionTimeout = "300",
        invokes = {@GroupInvoke(name = "updateTextContent", resultToContext = "true"), @GroupInvoke(name = "updateWorkEffortContent", resultToContext = "false")}
    )
    public interface UpdateWorkEffortTextContent {}

    /**
     * Upload and attach a file to a WorkEffort
     */
    @Service(
        name = "uploadWorkEffortContentFile",
        engine = "group",
        description = "Upload and attach a file to a WorkEffort",
        transactionTimeout = "300",
        invokes = {@GroupInvoke(name = "createContentFromUploadedFile", resultToContext = "true"), @GroupInvoke(name = "createWorkEffortContent", resultToContext = "false")}
    )
    public interface UploadWorkEffortContentFile {}

    /**
     * Create a Work Effort Review
     */
    @Service(
        name = "createWorkEffortReview",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "createWorkEffortReview",
        description = "Create a Work Effort Review",
        defaultEntityName = "WorkEffortReview",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "workEffortManagerPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface CreateWorkEffortReview {}

    /**
     * Update a Work Effort Review
     */
    @Service(
        name = "updateWorkEffortReview",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "updateWorkEffortReview",
        description = "Update a Work Effort Review",
        defaultEntityName = "WorkEffortReview",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "workEffortManagerPermission", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "reviewDate", optional = "false"),
            @OverrideAttribute(name = "userLoginId", optional = "false"),
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface UpdateWorkEffortReview {}

    /**
     * Remove a Work Effort Review
     */
    @Service(
        name = "deleteWorkEffortReview",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "deleteWorkEffortReview",
        description = "Remove a Work Effort Review",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "WorkEffortReview", mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "workEffortManagerPermission", mainAction = "DELETE"),
        overrideAttributes = {
            @OverrideAttribute(name = "reviewDate", optional = "false"),
            @OverrideAttribute(name = "userLoginId", optional = "false"),
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface DeleteWorkEffortReview {}

    /**
     * Index the Keywords for a WorkEffort
     */
    @Service(
        name = "indexWorkEffortKeywords",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "indexWorkEffortKeywords",
        description = "Index the Keywords for a WorkEffort",
        attributes = {
            @Attribute(name = "workEffortId", type = "String", mode = "IN"),
            @Attribute(name = "workEffortInstance", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface IndexWorkEffortKeywords {}

    /**
     * Create a Work Effort Keyword
     */
    @Service(
        name = "createWorkEffortKeyword",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "createWorkEffortKeyword",
        description = "Create a Work Effort Keyword",
        defaultEntityName = "WorkEffortKeyword",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "workEffortManagerPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "keyword", optional = "false"),
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface CreateWorkEffortKeyword {}

    /**
     * Remove a Work Effort Keyword
     */
    @Service(
        name = "deleteWorkEffortKeyword",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "deleteWorkEffortKeyword",
        description = "Remove a Work Effort Keyword",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "WorkEffortKeyword", mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "workEffortManagerPermission", mainAction = "DELETE"),
        overrideAttributes = {
            @OverrideAttribute(name = "keyword", optional = "false"),
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface DeleteWorkEffortKeyword {}

    /**
     * Create a Work Effort Keyword
     */
    @Service(
        name = "createWorkEffortKeywords",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "createWorkEffortKeywords",
        description = "Create a Work Effort Keyword",
        defaultEntityName = "WorkEffortKeyword",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "workEffortManagerPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "keyword", optional = "false"),
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface CreateWorkEffortKeywords {}

    /**
     * Remove all Work Effort Keyword
     */
    @Service(
        name = "deleteWorkEffortKeywords",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "deleteWorkEffortKeywords",
        description = "Remove all Work Effort Keyword",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "WorkEffort", mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "workEffortManagerPermission", mainAction = "DELETE"),
        overrideAttributes = {
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface DeleteWorkEffortKeywords {}

    @Service(
        name = "workEffortManagerPermission",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/permission/WorkEffortPermissionServices.xml",
        invoke = "workEffortManagerPermission",
        implemented = {@Implements(service = "permissionInterface")}
    )
    public interface WorkEffortManagerPermission {}

    @Service(
        name = "workEffortGenericPermission",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/permission/WorkEffortPermissionServices.xml",
        invoke = "workEffortGenericPermission",
        implemented = {@Implements(service = "permissionInterface")},
        attributes = {
            @Attribute(name = "workEffortId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "workEffortParentId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface WorkEffortGenericPermission {}

    @Service(
        name = "timesheetUpdatePermission",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/permission/WorkEffortPermissionServices.xml",
        invoke = "timesheetUpdatePermission",
        implemented = {@Implements(service = "permissionInterface")},
        attributes = {
            @Attribute(name = "workEffortId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface TimesheetUpdatePermission {}

    /**
     * Create WorkEffortSkillStandard
     */
    @Service(
        name = "createWorkEffortSkillStandard",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "createWorkEffortSkillStandard",
        description = "Create WorkEffortSkillStandard",
        defaultEntityName = "WorkEffortSkillStandard",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "skillTypeId", optional = "false"),
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface CreateWorkEffortSkillStandard {}

    /**
     * Update WorkEffortSkillStandard
     */
    @Service(
        name = "updateWorkEffortSkillStandard",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "updateWorkEffortSkillStandard",
        description = "Update WorkEffortSkillStandard",
        defaultEntityName = "WorkEffortSkillStandard",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "skillTypeId", optional = "false"),
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface UpdateWorkEffortSkillStandard {}

    /**
     * Delete WorkEffortSkillStandard
     */
    @Service(
        name = "deleteWorkEffortSkillStandard",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "deleteWorkEffortSkillStandard",
        description = "Delete WorkEffortSkillStandard",
        defaultEntityName = "WorkEffortSkillStandard",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "skillTypeId", optional = "false"),
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface DeleteWorkEffortSkillStandard {}

    /**
     * Create a WorkEffort Attribute
     */
    @Service(
        name = "createWorkEffortAttribute",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a WorkEffort Attribute",
        defaultEntityName = "WorkEffortAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "workEffortGenericPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "attrName", optional = "false"),
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface CreateWorkEffortAttribute {}

    /**
     * Update a WorkEffort Attribute
     */
    @Service(
        name = "updateWorkEffortAttribute",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a WorkEffort Attribute",
        defaultEntityName = "WorkEffortAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "workEffortGenericPermission", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "attrName", optional = "false"),
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface UpdateWorkEffortAttribute {}

    /**
     * Delete a WorkEffort Attribute
     */
    @Service(
        name = "deleteWorkEffortAttribute",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a WorkEffort Attribute",
        defaultEntityName = "WorkEffortAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "workEffortGenericPermission", mainAction = "DELETE"),
        overrideAttributes = {
            @OverrideAttribute(name = "attrName", optional = "false"),
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface DeleteWorkEffortAttribute {}

    /**
     * Create WorkEffortContactMech; if contactMechId is not provided, a new contact mech is created (if partyId is set then the new contact mech is also associated to the party)
     */
    @Service(
        name = "createWorkEffortContactMech",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "createWorkEffortContactMech",
        description = "Create WorkEffortContactMech; if contactMechId is not provided, a new contact mech is created (if partyId is set then the new contact mech is also associated to the party)",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "WorkEffortContactMech", mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(entityName = "PartyContactMech", mode = "IN", optional = "true")
        },
        attributes = {
            @Attribute(name = "workEffortId", type = "String", mode = "IN"),
            @Attribute(name = "contactMechTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "infoString", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contactMechPurposeTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contactMechId", type = "String", mode = "INOUT", optional = "true")
        },
        permissionService = @PermissionService(service = "workEffortGenericPermission", mainAction = "CREATE")
    )
    public interface CreateWorkEffortContactMech {}

    /**
     * Update WorkEffortContactMech
     */
    @Service(
        name = "updateWorkEffortContactMech",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "updateWorkEffortContactMech",
        description = "Update WorkEffortContactMech",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "WorkEffortContactMech", mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(entityName = "PartyContactMech", mode = "IN", optional = "true")
        },
        attributes = {
            @Attribute(name = "workEffortId", type = "String", mode = "IN"),
            @Attribute(name = "contactMechTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "infoString", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contactMechPurposeTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "newContactMechId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contactMechId", type = "String", mode = "INOUT", optional = "true")
        },
        permissionService = @PermissionService(service = "workEffortGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateWorkEffortContactMech {}

    /**
     * Delete WorkEffortContactMech
     */
    @Service(
        name = "deleteWorkEffortContactMech",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "deleteWorkEffortContactMech",
        description = "Delete WorkEffortContactMech",
        defaultEntityName = "WorkEffortContactMech",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "workEffortGenericPermission", mainAction = "DELETE")
    )
    public interface DeleteWorkEffortContactMech {}

    /**
     * Create WorkEffort PostalAddress
     */
    @Service(
        name = "createWorkEffortPostalAddress",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "createWorkEffortPostalAddress",
        description = "Create WorkEffort PostalAddress",
        implemented = {@Implements(service = "createPostalAddress"), @Implements(service = "createPartyPostalAddress"), @Implements(service = "createWorkEffortContactMech")},
        permissionService = @PermissionService(service = "workEffortGenericPermission", mainAction = "CREATE")
    )
    public interface CreateWorkEffortPostalAddress {}

    /**
     * Update WorkEffort PostalAddress
     */
    @Service(
        name = "updateWorkEffortPostalAddress",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "updateWorkEffortPostalAddress",
        description = "Update WorkEffort PostalAddress",
        implemented = {@Implements(service = "updatePostalAddress"), @Implements(service = "updateWorkEffortContactMech")},
        permissionService = @PermissionService(service = "workEffortGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateWorkEffortPostalAddress {}

    /**
     * Create WorkEffort TelecomNumber
     */
    @Service(
        name = "createWorkEffortTelecomNumber",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "createWorkEffortTelecomNumber",
        description = "Create WorkEffort TelecomNumber",
        implemented = {@Implements(service = "createTelecomNumber"), @Implements(service = "createPartyTelecomNumber"), @Implements(service = "createWorkEffortContactMech")},
        permissionService = @PermissionService(service = "workEffortGenericPermission", mainAction = "CREATE")
    )
    public interface CreateWorkEffortTelecomNumber {}

    /**
     * Update WorkEffort TelecomNumber
     */
    @Service(
        name = "updateWorkEffortTelecomNumber",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "updateWorkEffortTelecomNumber",
        description = "Update WorkEffort TelecomNumber",
        implemented = {@Implements(service = "updateTelecomNumber"), @Implements(service = "updateWorkEffortContactMech")},
        permissionService = @PermissionService(service = "workEffortGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateWorkEffortTelecomNumber {}

    /**
     * Create WorkEffort Email Address
     */
    @Service(
        name = "createWorkEffortEmailAddress",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "createWorkEffortEmailAddress",
        description = "Create WorkEffort Email Address",
        implemented = {@Implements(service = "createWorkEffortContactMech")},
        permissionService = @PermissionService(service = "workEffortGenericPermission", mainAction = "CREATE")
    )
    public interface CreateWorkEffortEmailAddress {}

    /**
     * Update WorkEffort Email Address
     */
    @Service(
        name = "updateWorkEffortEmailAddress",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "updateWorkEffortEmailAddress",
        description = "Update WorkEffort Email Address",
        implemented = {@Implements(service = "updateWorkEffortContactMech")},
        permissionService = @PermissionService(service = "workEffortGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateWorkEffortEmailAddress {}

    /**
     * Create WorkEffortInventoryProduced
     */
    @Service(
        name = "createWorkEffortInventoryProduced",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "createWorkEffortInventoryProduced",
        description = "Create WorkEffortInventoryProduced",
        defaultEntityName = "WorkEffortInventoryProduced",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "inventoryItemId", optional = "false"),
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface CreateWorkEffortInventoryProduced {}

    /**
     * Delete WorkEffortInventoryProduced
     */
    @Service(
        name = "deleteWorkEffortInventoryProduced",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "deleteWorkEffortInventoryProduced",
        description = "Delete WorkEffortInventoryProduced",
        defaultEntityName = "WorkEffortInventoryProduced",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "workEffortId", optional = "false"),
            @OverrideAttribute(name = "inventoryItemId", optional = "false")
        }
    )
    public interface DeleteWorkEffortInventoryProduced {}

    /**
     * Create WorkEffort iCalendar Data
     */
    @Service(
        name = "createWorkEffortICalData",
        engine = "entity-auto",
        invoke = "create",
        description = "Create WorkEffort iCalendar Data",
        defaultEntityName = "WorkEffortIcalData",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "workEffortICalendarPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface CreateWorkEffortICalData {}

    /**
     * Update WorkEffort iCalendar Data
     */
    @Service(
        name = "updateWorkEffortICalData",
        engine = "entity-auto",
        invoke = "update",
        description = "Update WorkEffort iCalendar Data",
        defaultEntityName = "WorkEffortIcalData",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "workEffortICalendarPermission", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface UpdateWorkEffortICalData {}

    /**
     * Delete WorkEffort iCalendar Data
     */
    @Service(
        name = "deleteWorkEffortICalData",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete WorkEffort iCalendar Data",
        defaultEntityName = "WorkEffortIcalData",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "workEffortICalendarPermission", mainAction = "DELETE"),
        overrideAttributes = {
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface DeleteWorkEffortICalData {}

    /**
     * iCalendar Permission Check
     */
    @Service(
        name = "workEffortICalendarPermission",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/permission/WorkEffortPermissionServices.xml",
        invoke = "workEffortICalendarPermission",
        description = "iCalendar Permission Check",
        implemented = {@Implements(service = "permissionInterface")},
        attributes = {
            @Attribute(name = "workEffortId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface WorkEffortICalendarPermission {}

    /**
     * Get iCalendar Work Efforts
     */
    @Service(
        name = "getICalWorkEfforts",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "getICalWorkEfforts",
        description = "Get iCalendar Work Efforts",
        attributes = {
            @Attribute(name = "workEffortId", type = "String", mode = "IN"),
            @Attribute(name = "workEffortTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "workEfforts", type = "List", mode = "OUT")
        }
    )
    public interface GetICalWorkEfforts {}

    /**
     * Get Party iCalendar URL
     */
    @Service(
        name = "getPartyICalUrl",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "getPartyICalUrl",
        description = "Get Party iCalendar URL",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "iCalUrl", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface GetPartyICalUrl {}

    /**
     * Create a WorkEffort Event Reminder
     */
    @Service(
        name = "createWorkEffortEventReminder",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a WorkEffort Event Reminder",
        defaultEntityName = "WorkEffortEventReminder",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "workEffortGenericPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "sequenceId", mode = "OUT"),
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface CreateWorkEffortEventReminder {}

    /**
     * Update a WorkEffort Event Reminder
     */
    @Service(
        name = "updateWorkEffortEventReminder",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a WorkEffort Event Reminder",
        defaultEntityName = "WorkEffortEventReminder",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "workEffortGenericPermission", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "sequenceId", optional = "false"),
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface UpdateWorkEffortEventReminder {}

    /**
     * Delete a WorkEffort Event Reminder
     */
    @Service(
        name = "deleteWorkEffortEventReminder",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a WorkEffort Event Reminder",
        defaultEntityName = "WorkEffortEventReminder",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "workEffortGenericPermission", mainAction = "DELETE"),
        overrideAttributes = {
            @OverrideAttribute(name = "sequenceId", optional = "false"),
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface DeleteWorkEffortEventReminder {}

    /**
     * Process work effort event reminders. This service is run by the job scheduler.
     */
    @Service(
        name = "processWorkEffortEventReminders",
        engine = "java",
        location = "org.ofbiz.workeffort.workeffort.WorkEffortServices",
        invoke = "processWorkEffortEventReminders",
        description = "Process work effort event reminders. This service is run by the job scheduler.",
        auth = "true"
    )
    public interface ProcessWorkEffortEventReminders {}

    /**
     * Send a work effort event reminder
     */
    @Service(
        name = "processWorkEffortEventReminder",
        engine = "java",
        location = "org.ofbiz.workeffort.workeffort.WorkEffortServices",
        invoke = "processWorkEffortEventReminder",
        description = "Send a work effort event reminder",
        auth = "true",
        attributes = {
            @Attribute(name = "reminder", type = "GenericValue", mode = "IN"),
            @Attribute(name = "bodyParameters", type = "Map", mode = "IN")
        }
    )
    public interface ProcessWorkEffortEventReminder {}

    /**
     * Migrate work effort event reminders. Run this service to update work effort reminders.
     */
    @Service(
        name = "migrateWorkEffortEventReminders",
        engine = "java",
        location = "org.ofbiz.workeffort.workeffort.WorkEffortServices",
        invoke = "migrateWorkEffortEventReminders",
        description = "Migrate work effort event reminders. Run this service to update work effort reminders.",
        auth = "true"
    )
    public interface MigrateWorkEffortEventReminders {}

    /**
     * Create a WorkEffort Survey
     */
    @Service(
        name = "createWorkEffortSurveyAppl",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "createWorkEffortSurveyAppl",
        description = "Create a WorkEffort Survey",
        defaultEntityName = "WorkEffortSurveyAppl",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "workEffortGenericPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "true"),
            @OverrideAttribute(name = "surveyId", optional = "false"),
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface CreateWorkEffortSurveyAppl {}

    /**
     * Update a WorkEffort Survey
     */
    @Service(
        name = "updateWorkEffortSurveyAppl",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "updateWorkEffortSurveyAppl",
        description = "Update a WorkEffort Survey",
        defaultEntityName = "WorkEffortSurveyAppl",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "workEffortGenericPermission", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "false"),
            @OverrideAttribute(name = "surveyId", optional = "false"),
            @OverrideAttribute(name = "workEffortId", optional = "false")
        }
    )
    public interface UpdateWorkEffortSurveyAppl {}

    /**
     * Delete a WorkEffort Survey
     */
    @Service(
        name = "deleteWorkEffortSurveyAppl",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml",
        invoke = "deleteWorkEffortSurveyAppl",
        description = "Delete a WorkEffort Survey",
        defaultEntityName = "WorkEffortSurveyAppl",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "workEffortGenericPermission", mainAction = "DELETE")
    )
    public interface DeleteWorkEffortSurveyAppl {}

}

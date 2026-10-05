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
package com.ilscipio.scipio.service.service;

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
     * Cleans out old jobs which have been around longer then what is defined in serviceengine.xml
     */
    @Service(
        name = "purgeOldJobs",
        location = "org.ofbiz.service.ServiceUtil",
        invoke = "purgeOldJobs",
        description = "Cleans out old jobs which have been around longer then what is defined in serviceengine.xml",
        auth = "true",
        useTransaction = "false",
        semaphore = "wait",
        permissions = {@Permissions(joinType = "AND", permissions = {@Permission(permission = "SERVICE_INVOKE_ANY")})}
    )
    public interface PurgeOldJobs {}

    /**
     * Cancels a schedule job
     */
    @Service(
        name = "cancelScheduledJob",
        location = "org.ofbiz.service.ServiceUtil",
        invoke = "cancelJob",
        description = "Cancels a schedule job",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "JobSandbox", mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "cancelDateTime", type = "Timestamp", mode = "OUT"),
            @Attribute(name = "statusId", type = "String", mode = "OUT")
        },
        permissions = {@Permissions(joinType = "AND", permissions = {@Permission(permission = "SERVICE_INVOKE_ANY")})}
    )
    public interface CancelScheduledJob {}

    /**
     * Cancels a job retry flag
     */
    @Service(
        name = "cancelJobRetries",
        location = "org.ofbiz.service.ServiceUtil",
        invoke = "cancelJobRetries",
        description = "Cancels a job retry flag",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "JobSandbox", mode = "IN", include = "pk")
        },
        permissions = {@Permissions(joinType = "AND", permissions = {@Permission(permission = "SERVICE_INVOKE_ANY")})}
    )
    public interface CancelJobRetries {}

    /**
     * Resets a stale job so it can be re-run
     */
    @Service(
        name = "resetScheduledJob",
        location = "org.ofbiz.service.ServiceUtil",
        invoke = "resetJob",
        description = "Resets a stale job so it can be re-run",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "JobSandbox", mode = "IN", include = "pk")
        },
        permissions = {@Permissions(joinType = "AND", permissions = {@Permission(permission = "SERVICE_INVOKE_ANY")})}
    )
    public interface ResetScheduledJob {}

    /**
     * Interface to describe base parameters for Permission Services
     */
    @Service(
        name = "permissionInterface",
        engine = "interface",
        description = "Interface to describe base parameters for Permission Services",
        attributes = {
            @Attribute(name = "mainAction", type = "String", mode = "IN", optional = "true", description = "The action requiring permission. Must be one of ADMIN, CREATE, UPDATE, DELETE, VIEW."),
            @Attribute(name = "primaryPermission", type = "String", mode = "IN", optional = "true", description = "The permission to check - typically the name of an application or entity."),
            @Attribute(name = "altPermission", type = "String", mode = "IN", optional = "true", description = "Optional alternate permission to check. If the primary permission check fails,\n            the alternate permission will be checked."),
            @Attribute(name = "resourceDescription", type = "String", mode = "IN", optional = "true", description = "The name of the resource being accessed - defaults to service name."),
            @Attribute(name = "hasPermission", type = "Boolean", mode = "OUT", description = "Contains true if the requested permission has been granted."),
            @Attribute(name = "failMessage", type = "String", mode = "OUT", optional = "true", description = "Contains an explanation if the permission was denied.")
        }
    )
    public interface PermissionInterface {}

    /**
     * Interface to describe authentication services
     */
    @Service(
        name = "authenticationInterface",
        engine = "interface",
        description = "Interface to describe authentication services",
        attributes = {
            @Attribute(name = "login.username", type = "String", mode = "IN"),
            @Attribute(name = "login.password", type = "String", mode = "IN"),
            @Attribute(name = "visitId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "isServiceAuth", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "userLogin", type = "org.ofbiz.entity.GenericValue", mode = "OUT"),
            @Attribute(name = "userLoginSession", type = "java.util.Map", mode = "OUT", optional = "true")
        }
    )
    public interface AuthenticationInterface {}

    /**
     * Interface to describe services call with streams
     */
    @Service(
        name = "serviceStreamInterface",
        engine = "interface",
        description = "Interface to describe services call with streams",
        attributes = {
            @Attribute(name = "inputStream", type = "java.io.InputStream", mode = "IN"),
            @Attribute(name = "outputStream", type = "java.io.OutputStream", mode = "IN"),
            @Attribute(name = "contentType", type = "String", mode = "OUT")
        }
    )
    public interface ServiceStreamInterface {}

    /**
     * Interface to describe services which are used as SECA conditions
     */
    @Service(
        name = "serviceEcaConditionInterface",
        engine = "interface",
        description = "Interface to describe services which are used as SECA conditions",
        attributes = {
            @Attribute(name = "serviceContext", type = "Map", mode = "IN"),
            @Attribute(name = "serviceName", type = "String", mode = "IN"),
            @Attribute(name = "conditionReply", type = "Boolean", mode = "OUT")
        }
    )
    public interface ServiceEcaConditionInterface {}

    /**
     * Interface to describe services which are used as SMCA conditions
     */
    @Service(
        name = "serviceMcaConditionInterface",
        engine = "interface",
        description = "Interface to describe services which are used as SMCA conditions",
        attributes = {
            @Attribute(name = "messageWrapper", type = "org.ofbiz.service.mail.MimeMessageWrapper", mode = "IN"),
            @Attribute(name = "conditionReply", type = "Boolean", mode = "OUT")
        }
    )
    public interface ServiceMcaConditionInterface {}

    /**
     * Interface to describe services used to process incoming email
     */
    @Service(
        name = "mailProcessInterface",
        engine = "interface",
        description = "Interface to describe services used to process incoming email",
        attributes = {
            @Attribute(name = "messageWrapper", type = "org.ofbiz.service.mail.MimeMessageWrapper", mode = "IN")
        }
    )
    public interface MailProcessInterface {}

    @Service(
        name = "effectiveDateEcaCondition",
        location = "org.ofbiz.service.ServiceUtil",
        invoke = "genericDateCondition",
        useTransaction = "false",
        implemented = {@Implements(service = "serviceEcaConditionInterface")},
        attributes = {
            @Attribute(name = "fromDate", type = "java.sql.Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "thruDate", type = "java.sql.Timestamp", mode = "IN", optional = "true")
        }
    )
    public interface EffectiveDateEcaCondition {}

    /**
     * Create a Job Manager Lock
     */
    @Service(
        name = "createJobManagerLock",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a Job Manager Lock",
        defaultEntityName = "JobManagerLock",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "commonGenericPermission", mainAction = "CREATE")
    )
    public interface CreateJobManagerLock {}

    /**
     * Cancel a Job Sandbox Lock
     */
    @Service(
        name = "updateJobManagerLock",
        engine = "entity-auto",
        invoke = "update",
        description = "Cancel a Job Sandbox Lock",
        defaultEntityName = "JobManagerLock",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "commonGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateJobManagerLock {}

    /**
     * Create a Catalina Session Record
     */
    @Service(
        name = "createCatalinaSession",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a Catalina Session Record",
        defaultEntityName = "CatalinaSession",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateCatalinaSession {}

    /**
     * Update a Catalina Session Record
     */
    @Service(
        name = "updateCatalinaSession",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Catalina Session Record",
        defaultEntityName = "CatalinaSession",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateCatalinaSession {}

    /**
     * Delete a Catalina Session Record
     */
    @Service(
        name = "deleteCatalinaSession",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a Catalina Session Record",
        defaultEntityName = "CatalinaSession",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface DeleteCatalinaSession {}

    /**
     * Create a StandardLanguage Record
     */
    @Service(
        name = "createStandardLanguage",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a StandardLanguage Record",
        defaultEntityName = "StandardLanguage",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateStandardLanguage {}

    /**
     * Update a StandardLanguage Record
     */
    @Service(
        name = "updateStandardLanguage",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a StandardLanguage Record",
        defaultEntityName = "StandardLanguage",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateStandardLanguage {}

    /**
     * Delete a StandardLanguage Record
     */
    @Service(
        name = "deleteStandardLanguage",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a StandardLanguage Record",
        defaultEntityName = "StandardLanguage",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteStandardLanguage {}

    /**
     * Create a StatusItem Record
     */
    @Service(
        name = "createStatusItem",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a StatusItem Record",
        defaultEntityName = "StatusItem",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateStatusItem {}

    /**
     * Update a StatusItem Record
     */
    @Service(
        name = "updateStatusItem",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a StatusItem Record",
        defaultEntityName = "StatusItem",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateStatusItem {}

    /**
     * Delete a StatusItem Record
     */
    @Service(
        name = "deleteStatusItem",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a StatusItem Record",
        defaultEntityName = "StatusItem",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteStatusItem {}

    /**
     * Create a StatusType
     */
    @Service(
        name = "createStatusType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a StatusType",
        defaultEntityName = "StatusType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateStatusType {}

    /**
     * Update a StatusType
     */
    @Service(
        name = "updateStatusType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a StatusType",
        defaultEntityName = "StatusType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateStatusType {}

    /**
     * Delete a StatusType
     */
    @Service(
        name = "deleteStatusType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a StatusType",
        defaultEntityName = "StatusType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteStatusType {}

    /**
     * Create a SequenceValueItem
     */
    @Service(
        name = "createSequenceValueItem",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a SequenceValueItem",
        defaultEntityName = "SequenceValueItem",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateSequenceValueItem {}

    /**
     * Update a SequenceValueItem
     */
    @Service(
        name = "updateSequenceValueItem",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a SequenceValueItem",
        defaultEntityName = "SequenceValueItem",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateSequenceValueItem {}

    /**
     * Delete a SequenceValueItem
     */
    @Service(
        name = "deleteSequenceValueItem",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a SequenceValueItem",
        defaultEntityName = "SequenceValueItem",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteSequenceValueItem {}

    /**
     * Create JobSandbox record
     */
    @Service(
        name = "createJobSandbox",
        engine = "entity-auto",
        invoke = "create",
        description = "Create JobSandbox record",
        defaultEntityName = "JobSandbox",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "commonGenericPermission", mainAction = "CREATE")
    )
    public interface CreateJobSandbox {}

    /**
     * Update JobSandbox record
     */
    @Service(
        name = "updateJobSandbox",
        engine = "entity-auto",
        invoke = "update",
        description = "Update JobSandbox record",
        defaultEntityName = "JobSandbox",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "commonGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateJobSandbox {}

    /**
     * Delete JobSandbox record
     */
    @Service(
        name = "deleteJobSandbox",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete JobSandbox record",
        defaultEntityName = "JobSandbox",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "commonGenericPermission", mainAction = "DELETE")
    )
    public interface DeleteJobSandbox {}

    /**
     * Create RuntimeData record
     */
    @Service(
        name = "createRuntimeData",
        engine = "entity-auto",
        invoke = "create",
        description = "Create RuntimeData record",
        defaultEntityName = "RuntimeData",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "commonGenericPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "runtimeInfo", allowHtml = "any")
        }
    )
    public interface CreateRuntimeData {}

    /**
     * Update RuntimeData record
     */
    @Service(
        name = "updateRuntimeData",
        engine = "entity-auto",
        invoke = "update",
        description = "Update RuntimeData record",
        defaultEntityName = "RuntimeData",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "commonGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateRuntimeData {}

    /**
     * Delete RuntimeData record
     */
    @Service(
        name = "deleteRuntimeData",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete RuntimeData record",
        defaultEntityName = "RuntimeData",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "commonGenericPermission", mainAction = "DELETE")
    )
    public interface DeleteRuntimeData {}

    /**
     * A special interface that services may implement to receive details about the job that triggered the service
     */
    @Service(
        name = "scipioJobCtxInterface",
        engine = "interface",
        description = "A special interface that services may implement to receive details about the job that triggered the service",
        attributes = {
            @Attribute(name = "scipioJobCtx", type = "Map", mode = "IN", optional = "true", description = "Contains the following fields:\n                * eventId - special Scipio eventId (SCH_EVENT_STARTUP, ...)")
        }
    )
    public interface ScipioJobCtxInterface {}

    /**
     * A special interface that services may implement to set basic user/password auth needed while creating connections to remote services/hosts
     */
    @Service(
        name = "scipioClientNamePassAuthInterface",
        engine = "interface",
        description = "A special interface that services may implement to set basic user/password auth needed while creating connections to remote services/hosts",
        attributes = {
            @Attribute(name = "clientLogin.username", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "clientLogin.password", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface ScipioClientNamePassAuthInterface {}

    /**
     * A special interface that services may implement to set auth needed while creating connections to remote services/hosts
     */
    @Service(
        name = "scipioClientAuthInterface",
        engine = "interface",
        description = "A special interface that services may implement to set auth needed while creating connections to remote services/hosts",
        implemented = {@Implements(service = "scipioClientNamePassAuthInterface")}
    )
    public interface ScipioClientAuthInterface {}

    /**
     * A special interface that services may implement to set specific attributes for SOAP based services
     */
    @Service(
        name = "scipioSOAPInterface",
        engine = "interface",
        description = "A special interface that services may implement to set specific attributes for SOAP based services",
        attributes = {
            @Attribute(name = "soapServiceInvokerClass", type = "Class", mode = "IN", optional = "true"),
            @Attribute(name = "soapServiceHeader", type = "java.util.List", mode = "IN", optional = "true"),
            @Attribute(name = "soapServicePayload", type = "Object", mode = "IN", optional = "true"),
            @Attribute(name = "soapResponseDocument", type = "org.w3c.dom.Document", mode = "OUT", optional = "true")
        }
    )
    public interface ScipioSOAPInterface {}

}

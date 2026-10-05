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
package com.ilscipio.scipio.common.service;

import com.ilscipio.scipio.service.def.*;
import com.ilscipio.scipio.service.def.Service.GroupInvoke;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class SecurityServices {

    /**
     * Create an SecurityGroup
     */
    @Service(
        name = "createSecurityGroup",
        engine = "entity-auto",
        invoke = "create",
        description = "Create an SecurityGroup",
        defaultEntityName = "SecurityGroup",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "securityPermissionCheck", mainAction = "CREATE")
    )
    public interface CreateSecurityGroup {}

    /**
     * Update a SecurityGroup
     */
    @Service(
        name = "updateSecurityGroup",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a SecurityGroup",
        defaultEntityName = "SecurityGroup",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "securityPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateSecurityGroup {}

    /**
     * Create a SecurityPermission
     */
    @Service(
        name = "createSecurityPermission",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a SecurityPermission",
        defaultEntityName = "SecurityPermission",
        auth = "true",
        attributes = {
            @Attribute(name = "permissionId", type = "String", mode = "IN"),
            @Attribute(name = "description", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "securityPermissionCheck", mainAction = "CREATE")
    )
    public interface CreateSecurityPermission {}

    /**
     * Update a SecurityPermission
     */
    @Service(
        name = "updateSecurityPermission",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a SecurityPermission",
        defaultEntityName = "SecurityPermission",
        auth = "true",
        attributes = {
            @Attribute(name = "permissionId", type = "String", mode = "IN"),
            @Attribute(name = "description", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "securityPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateSecurityPermission {}

    /**
     * Add a SecurityPermission to a SecurityGroup
     */
    @Service(
        name = "addSecurityPermissionToSecurityGroup",
        engine = "entity-auto",
        invoke = "create",
        description = "Add a SecurityPermission to a SecurityGroup",
        defaultEntityName = "SecurityGroupPermission",
        auth = "true",
        attributes = {
            @Attribute(name = "groupId", type = "String", mode = "IN"),
            @Attribute(name = "permissionId", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "securityPermissionCheck", mainAction = "CREATE")
    )
    public interface AddSecurityPermissionToSecurityGroup {}

    /**
     * Update a SecurityPermission from a SecurityGroup
     */
    @Service(
        name = "updateSecurityPermissionToSecurityGroup",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a SecurityPermission from a SecurityGroup",
        defaultEntityName = "SecurityGroupPermission",
        auth = "true",
        attributes = {
            @Attribute(name = "groupId", type = "String", mode = "IN"),
            @Attribute(name = "permissionId", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "securityPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateSecurityPermissionToSecurityGroup {}

    /**
     * Remove a SecurityPermission from a SecurityGroup
     */
    @Service(
        name = "removeSecurityPermissionFromSecurityGroup",
        engine = "entity-auto",
        invoke = "delete",
        description = "Remove a SecurityPermission from a SecurityGroup",
        defaultEntityName = "SecurityGroupPermission",
        auth = "true",
        attributes = {
            @Attribute(name = "groupId", type = "String", mode = "IN"),
            @Attribute(name = "permissionId", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "securityPermissionCheck", mainAction = "DELETE")
    )
    public interface RemoveSecurityPermissionFromSecurityGroup {}

    /**
     * Add a UserLogin to a SecurityGroup
     */
    @Service(
        name = "addUserLoginToSecurityGroup",
        engine = "entity-auto",
        invoke = "create",
        description = "Add a UserLogin to a SecurityGroup",
        defaultEntityName = "UserLoginSecurityGroup",
        auth = "true",
        attributes = {
            @Attribute(name = "userLoginId", type = "String", mode = "IN"),
            @Attribute(name = "groupId", type = "String", mode = "IN"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "thruDate", type = "Timestamp", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "securityPermissionCheck", mainAction = "CREATE")
    )
    public interface AddUserLoginToSecurityGroup {}

    /**
     * Update a UserLogin to SecurityGroup Appl
     */
    @Service(
        name = "updateUserLoginToSecurityGroup",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a UserLogin to SecurityGroup Appl",
        defaultEntityName = "UserLoginSecurityGroup",
        auth = "true",
        attributes = {
            @Attribute(name = "userLoginId", type = "String", mode = "IN"),
            @Attribute(name = "groupId", type = "String", mode = "IN"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN"),
            @Attribute(name = "thruDate", type = "Timestamp", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "securityPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateUserLoginToSecurityGroup {}

    /**
     * Expire UserLoginSecurityGroup
     */
    @Service(
        name = "expireUserLoginSecurityGroup",
        engine = "entity-auto",
        invoke = "expire",
        description = "Expire UserLoginSecurityGroup",
        defaultEntityName = "UserLoginSecurityGroup",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "securityPermissionCheck", mainAction = "UPDATE")
    )
    public interface ExpireUserLoginSecurityGroup {}

    /**
     * Remove a UserLogin to SecurityGroup Appl
     */
    @Service(
        name = "removeUserLoginToSecurityGroup",
        engine = "entity-auto",
        invoke = "delete",
        description = "Remove a UserLogin to SecurityGroup Appl",
        defaultEntityName = "UserLoginSecurityGroup",
        auth = "true",
        attributes = {
            @Attribute(name = "userLoginId", type = "String", mode = "IN"),
            @Attribute(name = "groupId", type = "String", mode = "IN"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN"),
            @Attribute(name = "thruDate", type = "Timestamp", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "securityPermissionCheck", mainAction = "DELETE")
    )
    public interface RemoveUserLoginToSecurityGroup {}

    /**
     * Add a Protected View to a SecurityGroup
     */
    @Service(
        name = "addProtectedViewToSecurityGroup",
        engine = "entity-auto",
        invoke = "create",
        description = "Add a Protected View to a SecurityGroup",
        defaultEntityName = "ProtectedView",
        auth = "true",
        attributes = {
            @Attribute(name = "viewNameId", type = "String", mode = "IN"),
            @Attribute(name = "groupId", type = "String", mode = "IN"),
            @Attribute(name = "maxHits", type = "Long", mode = "IN"),
            @Attribute(name = "maxHitsDuration", type = "Long", mode = "IN"),
            @Attribute(name = "tarpitDuration", type = "Long", mode = "IN")
        },
        permissionService = @PermissionService(service = "securityPermissionCheck", mainAction = "CREATE")
    )
    public interface AddProtectedViewToSecurityGroup {}

    /**
     * Update a Protected View to SecurityGroup Assignment
     */
    @Service(
        name = "updateProtectedViewToSecurityGroup",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Protected View to SecurityGroup Assignment",
        defaultEntityName = "ProtectedView",
        auth = "true",
        attributes = {
            @Attribute(name = "viewNameId", type = "String", mode = "IN"),
            @Attribute(name = "groupId", type = "String", mode = "IN"),
            @Attribute(name = "maxHits", type = "Long", mode = "IN"),
            @Attribute(name = "maxHitsDuration", type = "Long", mode = "IN"),
            @Attribute(name = "tarpitDuration", type = "Long", mode = "IN")
        },
        permissionService = @PermissionService(service = "securityPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateProtectedViewToSecurityGroup {}

    /**
     * Remove a Protected View from a SecurityGroup
     */
    @Service(
        name = "removeProtectedViewFromSecurityGroup",
        engine = "entity-auto",
        invoke = "delete",
        description = "Remove a Protected View from a SecurityGroup",
        defaultEntityName = "ProtectedView",
        auth = "true",
        attributes = {
            @Attribute(name = "viewNameId", type = "String", mode = "IN"),
            @Attribute(name = "groupId", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "securityPermissionCheck", mainAction = "DELETE")
    )
    public interface RemoveProtectedViewFromSecurityGroup {}

    @Service(
        name = "securityPermissionCheck",
        engine = "simple",
        location = "component://common/script/org/ofbiz/common/permission/CommonPermissionServices.xml",
        invoke = "genericBasePermissionCheck",
        implemented = {@Implements(service = "permissionInterface")},
        attributes = {
            @Attribute(name = "primaryPermission", type = "String", mode = "IN", optional = "true", defaultValue = "SECURITY")
        }
    )
    public interface SecurityPermissionCheck {}

    /**
     * Create a UserLoginSecurityQuestion
     */
    @Service(
        name = "createUserLoginSecurityQuestion",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a UserLoginSecurityQuestion",
        defaultEntityName = "UserLoginSecurityQuestion",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateUserLoginSecurityQuestion {}

    /**
     * Update a UserLoginSecurityQuestion
     */
    @Service(
        name = "updateUserLoginSecurityQuestion",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a UserLoginSecurityQuestion",
        defaultEntityName = "UserLoginSecurityQuestion",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateUserLoginSecurityQuestion {}

    /**
     * Remove UserLoginSecurityQuestion
     */
    @Service(
        name = "removeUserLoginSecurityQuestion",
        engine = "entity-auto",
        invoke = "delete",
        description = "Remove UserLoginSecurityQuestion",
        defaultEntityName = "UserLoginSecurityQuestion",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface RemoveUserLoginSecurityQuestion {}

    /**
     * Delete a SecurityGroup
     */
    @Service(
        name = "deleteSecurityGroup",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a SecurityGroup",
        defaultEntityName = "SecurityGroup",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "securityPermissionCheck", mainAction = "DELETE")
    )
    public interface DeleteSecurityGroup {}

}

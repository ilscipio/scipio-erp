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
package com.ilscipio.scipio.security.entity;

import com.ilscipio.scipio.entity.def.*;

/**
 * Auto-generated annotation-based entity definitions.
 *
 * <p>Generated from entitymodel.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class Entities {

    /**
     * Valid issuer data for authentication of x.509 certificates
     */
    @Entity(
        name = "X509IssuerProvision",
        packageName = "org.ofbiz.security.cert",
        title = "Valid issuer data for authentication of x.509 certificates",
        neverCache = true,
        fields = {
            @Field(name = "certProvisionId", type = "id-ne"),
            @Field(name = "commonName", type = "value"),
            @Field(name = "organizationalUnit", type = "value"),
            @Field(name = "organizationName", type = "value"),
            @Field(name = "cityLocality", type = "value"),
            @Field(name = "stateProvince", type = "value"),
            @Field(name = "country", type = "value"),
            @Field(name = "serialNumber", type = "value")
        },
        primaryKeys = {
            @PrimaryKey(field = "certProvisionId")
        }
    )
    public interface X509IssuerProvisionEntity {}

    /**
     * User Login
     */
    @Entity(
        name = "UserLogin",
        packageName = "org.ofbiz.security.login",
        title = "User Login",
        fields = {
            @Field(name = "userLoginId", type = "id-vlong-ne"),
            @Field(name = "currentPassword", type = "long-varchar"),
            @Field(name = "passwordHint", type = "description"),
            @Field(name = "isSystem", type = "indicator"),
            @Field(name = "enabled", type = "indicator"),
            @Field(name = "hasLoggedOut", type = "indicator"),
            @Field(name = "requirePasswordChange", type = "indicator"),
            @Field(name = "lastCurrencyUom", type = "id"),
            @Field(name = "lastLocale", type = "very-short"),
            @Field(name = "lastTimeZone", type = "id-long"),
            @Field(name = "disabledDateTime", type = "date-time"),
            @Field(name = "successiveFailedLogins", type = "numeric"),
            @Field(name = "externalAuthId", type = "id-vlong-ne", description = "For use with external authentication; the userLdapDn should be replaced with this"),
            @Field(name = "userLdapDn", type = "id-vlong-ne", description = "The user's LDAP Distinguished Name - used for LDAP authentication"),
            @Field(name = "disabledBy", type = "id-vlong")
        },
        primaryKeys = {
            @PrimaryKey(field = "userLoginId")
        }
    )
    public interface UserLoginEntity {}

    /**
     * User Login Password History
     */
    @Entity(
        name = "UserLoginPasswordHistory",
        packageName = "org.ofbiz.security.login",
        title = "User Login Password History",
        neverCache = true,
        fields = {
            @Field(name = "userLoginId", type = "id-vlong-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "currentPassword", type = "long-varchar")
        },
        primaryKeys = {
            @PrimaryKey(field = "userLoginId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                fkName = "USER_LPH_USER",
                keyMaps = {
                    @KeyMap(fieldName = "userLoginId")
                }
            )
        }
    )
    public interface UserLoginPasswordHistoryEntity {}

    /**
     * User Login History
     */
    @Entity(
        name = "UserLoginHistory",
        packageName = "org.ofbiz.security.login",
        title = "User Login History",
        neverCache = true,
        fields = {
            @Field(name = "userLoginId", type = "id-vlong-ne"),
            @Field(name = "visitId", type = "id"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "passwordUsed", type = "long-varchar", encrypt = "true"),
            @Field(name = "successfulLogin", type = "indicator")
        },
        primaryKeys = {
            @PrimaryKey(field = "userLoginId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                fkName = "USER_LH_USER",
                keyMaps = {
                    @KeyMap(fieldName = "userLoginId")
                }
            )
        }
    )
    public interface UserLoginHistoryEntity {}

    /**
     * User Login History
     */
    @Entity(
        name = "UserLoginSession",
        packageName = "org.ofbiz.security.login",
        title = "User Login History",
        neverCache = true,
        fields = {
            @Field(name = "userLoginId", type = "id-vlong-ne"),
            @Field(name = "savedDate", type = "date-time"),
            @Field(name = "sessionData", type = "very-long")
        },
        primaryKeys = {
            @PrimaryKey(field = "userLoginId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                fkName = "USER_SESSION_USER",
                keyMaps = {
                    @KeyMap(fieldName = "userLoginId")
                }
            )
        }
    )
    public interface UserLoginSessionEntity {}

    /**
     * User Login Per-App Info
     */
    @Entity(
        name = "UserLoginAppInfo",
        packageName = "org.ofbiz.security.login",
        title = "User Login Per-App Info",
        neverCache = true,
        fields = {
            @Field(name = "userLoginId", type = "id-vlong-ne"),
            @Field(name = "appId", type = "id-vlong-ne"),
            @Field(name = "autoLoginAuthToken", type = "very-long"),
            @Field(name = "autoLoginAuthDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "userLoginId"),
            @PrimaryKey(field = "appId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                fkName = "USER_APP_INFO_USER",
                keyMaps = {
                    @KeyMap(fieldName = "userLoginId")
                }
            )
        }
    )
    public interface UserLoginAppInfoEntity {}

    /**
     * Security Component - Security Group
     */
    @Entity(
        name = "SecurityGroup",
        packageName = "org.ofbiz.security.securitygroup",
        title = "Security Component - Security Group",
        defaultResourceName = "SecurityEntityLabels",
        fields = {
            @Field(name = "groupId", type = "id-ne"),
            @Field(name = "groupName", type = "value"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "groupId")
        }
    )
    public interface SecurityGroupEntity {}

    /**
     * Security Component - Security Group Permission
     * Defines a permission available to a security group; there is no FK to SecurityPermission because we want to leave open the possibility of ad-hoc permissions, especially for the Entity Data Maintenance pages which have TONS of permissions
     */
    @Entity(
        name = "SecurityGroupPermission",
        packageName = "org.ofbiz.security.securitygroup",
        title = "Security Component - Security Group Permission",
        description = "Defines a permission available to a security group; there is no FK to SecurityPermission because we want to leave open the possibility of ad-hoc permissions, especially for the Entity Data Maintenance pages which have TONS of permissions",
        fields = {
            @Field(name = "groupId", type = "id-ne"),
            @Field(name = "permissionId", type = "id-long-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "groupId"),
            @PrimaryKey(field = "permissionId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SecurityGroup",
                fkName = "SEC_GRP_PERM_GRP",
                keyMaps = {
                    @KeyMap(fieldName = "groupId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "SecurityPermission",
                keyMaps = {
                    @KeyMap(fieldName = "permissionId")
                }
            )
        }
    )
    public interface SecurityGroupPermissionEntity {}

    /**
     * Security Component - Security Permission
     */
    @Entity(
        name = "SecurityPermission",
        packageName = "org.ofbiz.security.securitygroup",
        title = "Security Component - Security Permission",
        defaultResourceName = "SecurityEntityLabels",
        fields = {
            @Field(name = "permissionId", type = "id-long-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "permissionId")
        }
    )
    public interface SecurityPermissionEntity {}

    /**
     * Security Component - User Login Security Group
     * Maps a UserLogin to a security group
     */
    @Entity(
        name = "UserLoginSecurityGroup",
        packageName = "org.ofbiz.security.securitygroup",
        title = "Security Component - User Login Security Group",
        description = "Maps a UserLogin to a security group",
        fields = {
            @Field(name = "userLoginId", type = "id-vlong-ne"),
            @Field(name = "groupId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "userLoginId"),
            @PrimaryKey(field = "groupId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                fkName = "USER_SECGRP_USER",
                keyMaps = {
                    @KeyMap(fieldName = "userLoginId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SecurityGroup",
                fkName = "USER_SECGRP_GRP",
                keyMaps = {
                    @KeyMap(fieldName = "groupId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "SecurityGroupPermission",
                keyMaps = {
                    @KeyMap(fieldName = "groupId")
                }
            )
        }
    )
    public interface UserLoginSecurityGroupEntity {}

    /**
     * Security Component - Protected View
     * Defines views protected from data leakage
     */
    @Entity(
        name = "ProtectedView",
        packageName = "org.ofbiz.security.securitygroup",
        title = "Security Component - Protected View",
        description = "Defines views protected from data leakage",
        fields = {
            @Field(name = "groupId", type = "id-ne"),
            @Field(name = "viewNameId", type = "id-long-ne", description = "name of view to protect from data theft"),
            @Field(name = "maxHits", type = "numeric", description = "number of hits before tarpitting a login for a view"),
            @Field(name = "maxHitsDuration", type = "numeric", description = "period of time associated with maxHits (in seconds)"),
            @Field(name = "tarpitDuration", type = "numeric", description = "period of time a login will not be able to acces  this view again (in seconds)")
        },
        primaryKeys = {
            @PrimaryKey(field = "groupId"),
            @PrimaryKey(field = "viewNameId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SecurityGroup",
                fkName = "VIEW_SECGRP_GRP",
                keyMaps = {
                    @KeyMap(fieldName = "groupId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "SecurityGroupPermission",
                keyMaps = {
                    @KeyMap(fieldName = "groupId")
                }
            )
        }
    )
    public interface ProtectedViewEntity {}

    /**
     * Security Component - Protected View
     * Login View couple currently tarpitted : any access to the view for the login is denied
     */
    @Entity(
        name = "TarpittedLoginView",
        packageName = "org.ofbiz.security.securitygroup",
        title = "Security Component - Protected View",
        description = "Login View couple currently tarpitted : any access to the view for the login is denied",
        fields = {
            @Field(name = "viewNameId", type = "id-long-ne", description = "name of view protected from data theft"),
            @Field(name = "userLoginId", type = "id-vlong-ne"),
            @Field(name = "tarpitReleaseDateTime", type = "numeric", description = "Date/Time at which the login will gain anew access to the view (in milliseconds from midnight, January 1, 1970 UTC , 0 meaning no tarpit to allow the admin to free a view and to keep history")
        },
        primaryKeys = {
            @PrimaryKey(field = "viewNameId"),
            @PrimaryKey(field = "userLoginId")
        }
    )
    public interface TarpittedLoginViewEntity {}

    @Entity(
        name = "UserLoginSecurityQuestion",
        packageName = "org.ofbiz.security.login",
        fields = {
            @Field(name = "questionEnumId", type = "id"),
            @Field(name = "userLoginId", type = "id-vlong"),
            @Field(name = "securityAnswer", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "questionEnumId"),
            @PrimaryKey(field = "userLoginId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                fkName = "SECQ_ENUM",
                keyMaps = {
                    @KeyMap(fieldName = "questionEnumId", relFieldName = "enumId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                fkName = "ULGNSECQ_ULGN",
                keyMaps = {
                    @KeyMap(fieldName = "userLoginId")
                }
            )
        }
    )
    public interface UserLoginSecurityQuestionEntity {}

    /**
     * UserLogin And SecurityGroup View
     */
    @ViewEntity(
        name = "UserLoginAndSecurityGroup",
        packageName = "org.ofbiz.security.securitygroup",
        title = "UserLogin And SecurityGroup View",
        neverCache = true,
        members = {
            @MemberEntity(entityAlias = "ULSG", entityName = "UserLoginSecurityGroup"),
            @MemberEntity(entityAlias = "UL", entityName = "UserLogin")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "ULSG"),
            @AliasAll(entityAlias = "UL")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "ULSG",
                relEntityAlias = "UL",
                keyMaps = {
                    @KeyMap(fieldName = "userLoginId")
                }
            )
        }
    )
    public interface UserLoginAndSecurityGroupView {}

    /**
     * UserLogin And ProtectedView View
     */
    @ViewEntity(
        name = "UserLoginAndProtectedView",
        packageName = "org.ofbiz.security.securitygroup",
        title = "UserLogin And ProtectedView View",
        members = {
            @MemberEntity(entityAlias = "ULSGPV", entityName = "UserLoginSecurityGroup"),
            @MemberEntity(entityAlias = "PV", entityName = "ProtectedView")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "ULSGPV"),
            @AliasAll(entityAlias = "PV")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "ULSGPV",
                relEntityAlias = "PV",
                keyMaps = {
                    @KeyMap(fieldName = "groupId")
                }
            )
        }
    )
    public interface UserLoginAndProtectedViewView {}

}

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
package com.ilscipio.scipio.entity.entity;

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
     * Entity Key Store
     */
    @Entity(
        name = "EntityKeyStore",
        packageName = "org.ofbiz.entity.crypto",
        title = "Entity Key Store",
        fields = {
            @Field(name = "keyName", type = "id-vlong-ne"),
            @Field(name = "keyText", type = "long-varchar")
        },
        primaryKeys = {
            @PrimaryKey(field = "keyName")
        }
    )
    public interface EntityKeyStoreEntity {}

    /**
     * Sequence Value Item
     */
    @Entity(
        name = "SequenceValueItem",
        packageName = "org.ofbiz.entity.sequence",
        title = "Sequence Value Item",
        fields = {
            @Field(name = "seqName", type = "id-long-ne"),
            @Field(name = "seqId", type = "numeric")
        },
        primaryKeys = {
            @PrimaryKey(field = "seqName")
        }
    )
    public interface SequenceValueItemEntity {}

    /**
     * Java Resource
     */
    @Entity(
        name = "JavaResource",
        packageName = "org.ofbiz.entity",
        title = "Java Resource",
        fields = {
            @Field(name = "resourceName", type = "id-vlong"),
            @Field(name = "resourceValue", type = "byte-array")
        },
        primaryKeys = {
            @PrimaryKey(field = "resourceName")
        }
    )
    public interface JavaResourceEntity {}

    @Entity(
        name = "Tenant",
        packageName = "org.ofbiz.entity.tenant",
        fields = {
            @Field(name = "tenantId", type = "id-ne"),
            @Field(name = "tenantName", type = "name"),
            @Field(name = "initialPath", type = "value"),
            @Field(name = "disabled", type = "indicator", description = "Disabled if 'Y', defaults to 'N' (not disabled)."),
            @Field(name = "planId", type = "id", description = "SCIPIO: 4.0.0: Pooled runtime: plan of the store; general.properties tenant.plan.<planId>.* sets its limits."),
            @Field(name = "suspendedDate", type = "date-time", description = "SCIPIO: 4.0.0: Pooled runtime: set by suspendTenant, cleared by resumeTenant."),
            @Field(name = "suspendReason", type = "description", description = "SCIPIO: 4.0.0: Pooled runtime: reason of the last suspend.")
        },
        primaryKeys = {
            @PrimaryKey(field = "tenantId")
        }
    )
    public interface TenantEntity {}

    /**
     *              There should be one record for each tenant and each group-map for the active delegator.             The jdbc fields will override the datasource -> inline-jdbc values for the per-tenant delegator.         
     */
    @Entity(
        name = "TenantDataSource",
        packageName = "org.ofbiz.entity.tenant",
        description = "\n            There should be one record for each tenant and each group-map for the active delegator.\n            The jdbc fields will override the datasource -> inline-jdbc values for the per-tenant delegator.\n        ",
        fields = {
            @Field(name = "tenantId", type = "id-ne"),
            @Field(name = "entityGroupName", type = "name"),
            @Field(name = "jdbcUri", type = "long-varchar"),
            @Field(name = "jdbcUsername", type = "long-varchar"),
            @Field(name = "jdbcPassword", type = "long-varchar")
        },
        primaryKeys = {
            @PrimaryKey(field = "tenantId"),
            @PrimaryKey(field = "entityGroupName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Tenant",
                fkName = "TNTDTSRC_TNT",
                keyMaps = {
                    @KeyMap(fieldName = "tenantId")
                }
            )
        }
    )
    public interface TenantDataSourceEntity {}

    /**
     *              There should be one record for each tenant and each group-map for the active delegator.             The jdbc fields will override the datasource -> inline-jdbc values for the per-tenant delegator.         
     */
    @Entity(
        name = "TenantKeyEncryptingKey",
        packageName = "org.ofbiz.entity.tenant",
        description = "\n            There should be one record for each tenant and each group-map for the active delegator.\n            The jdbc fields will override the datasource -> inline-jdbc values for the per-tenant delegator.\n        ",
        fields = {
            @Field(name = "tenantId", type = "id-ne"),
            @Field(name = "kekText", type = "long-varchar")
        },
        primaryKeys = {
            @PrimaryKey(field = "tenantId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Tenant",
                fkName = "TNTKEK_TNT",
                keyMaps = {
                    @KeyMap(fieldName = "tenantId")
                }
            )
        }
    )
    public interface TenantKeyEncryptingKeyEntity {}

    /**
     * Component Entity
     */
    @Entity(
        name = "Component",
        packageName = "org.ofbiz.entity.tenant",
        description = "Component Entity",
        fields = {
            @Field(name = "componentName", type = "name"),
            @Field(name = "rootLocation", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "componentName")
        }
    )
    public interface ComponentEntity {}

    @Entity(
        name = "TenantComponent",
        packageName = "org.ofbiz.entity.tenant",
        fields = {
            @Field(name = "tenantId", type = "id-ne"),
            @Field(name = "componentName", type = "name"),
            @Field(name = "sequenceNum", type = "numeric")
        },
        primaryKeys = {
            @PrimaryKey(field = "componentName"),
            @PrimaryKey(field = "tenantId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Tenant",
                fkName = "TNTCOMP_TNT",
                keyMaps = {
                    @KeyMap(fieldName = "tenantId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Component",
                fkName = "COMP_CNT",
                keyMaps = {
                    @KeyMap(fieldName = "componentName")
                }
            )
        }
    )
    public interface TenantComponentEntity {}

    /**
     * Tenant and its Domain Name
     */
    @Entity(
        name = "TenantDomainName",
        packageName = "org.ofbiz.entity.tenant",
        title = "Tenant and its Domain Name",
        fields = {
            @Field(name = "tenantId", type = "id-ne"),
            @Field(name = "domainName", type = "long-varchar")
        },
        primaryKeys = {
            @PrimaryKey(field = "domainName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Tenant",
                fkName = "TNNT_DMNAM",
                keyMaps = {
                    @KeyMap(fieldName = "tenantId")
                }
            )
        }
    )
    public interface TenantDomainNameEntity {}

    /**
     * SCIPIO: 4.0.0: Pooled runtime: maps an MCP token id to its store (master table, G2).
     */
    @Entity(
        name = "McpTokenRoute",
        packageName = "org.ofbiz.entity.tenant",
        title = "MCP Token Route",
        fields = {
            @Field(name = "tokenId", type = "id-ne"),
            @Field(name = "tenantId", type = "id-ne"),
            @Field(name = "createdDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "tokenId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Tenant",
                fkName = "MCP_TKRT_TNNT",
                keyMaps = {
                    @KeyMap(fieldName = "tenantId")
                }
            )
        }
    )
    public interface McpTokenRouteEntity {}

}

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
package com.ilscipio.scipio.mcp.entity;

import com.ilscipio.scipio.entity.def.Entity;
import com.ilscipio.scipio.entity.def.Field;
import com.ilscipio.scipio.entity.def.Index;
import com.ilscipio.scipio.entity.def.IndexField;
import com.ilscipio.scipio.entity.def.KeyMap;
import com.ilscipio.scipio.entity.def.PrimaryKey;
import com.ilscipio.scipio.entity.def.Relation;
import com.ilscipio.scipio.entity.def.RelationType;

/**
 * SCIPIO: 4.0.0: MCP entities: access tokens, audit log and tool usage counters.
 */
public class McpEntities {

    /**
     * Bearer token bound to a UserLogin. The raw token is shown once; only its SHA-256 hash is stored.
     */
    @Entity(
        name = "McpAccessToken",
        packageName = "com.ilscipio.scipio.mcp",
        title = "MCP Access Token",
        fields = {
            @Field(name = "tokenId", type = "id-ne"),
            @Field(name = "userLoginId", type = "id-vlong"),
            @Field(name = "tokenName", type = "name"),
            @Field(name = "tokenHash", type = "long-varchar"),
            @Field(name = "tokenPrefix", type = "short-varchar"),
            @Field(name = "webapps", type = "long-varchar"),
            @Field(name = "readOnly", type = "indicator"),
            @Field(name = "expiresDate", type = "date-time"),
            @Field(name = "lastUsedDate", type = "date-time"),
            @Field(name = "disabled", type = "indicator"),
            @Field(name = "remoteAddrAllow", type = "long-varchar"),
            @Field(name = "maxOrderAmount", type = "currency-amount"),
            @Field(name = "description", type = "description"),
            @Field(name = "createdByUserLogin", type = "id-vlong"),
            @Field(name = "createdDate", type = "date-time"),
            @Field(name = "tenantId", type = "id", description = "SCIPIO: 4.0.0: Pooled runtime: store of the token (empty on the base delegator); rows stay attributable after an export (G5).")
        },
        primaryKeys = { @PrimaryKey(field = "tokenId") },
        relations = {
            @Relation(type = RelationType.ONE, relEntityName = "UserLogin", keyMaps = { @KeyMap(fieldName = "userLoginId") })
        },
        indexes = {
            @Index(name = "MCP_TOKEN_USER_IDX", fields = { @IndexField(name = "userLoginId") })
        }
    )
    public interface McpAccessTokenEntity {}

    /**
     * One row per MCP tool call (also denied and failed calls). Written in its own transaction.
     */
    @Entity(
        name = "McpAuditLog",
        packageName = "com.ilscipio.scipio.mcp",
        title = "MCP Audit Log",
        fields = {
            @Field(name = "auditId", type = "id-ne"),
            @Field(name = "tokenId", type = "id-ne"),
            @Field(name = "userLoginId", type = "id-vlong"),
            @Field(name = "webappName", type = "name"),
            @Field(name = "serverName", type = "name"),
            @Field(name = "method", type = "name"),
            @Field(name = "toolName", type = "name"),
            @Field(name = "argsSummary", type = "very-long"),
            @Field(name = "status", type = "short-varchar"),
            @Field(name = "errorMessage", type = "very-long"),
            @Field(name = "durationMs", type = "numeric"),
            @Field(name = "remoteAddr", type = "short-varchar"),
            @Field(name = "requestId", type = "id-vlong"),
            @Field(name = "idempotencyKey", type = "long-varchar"),
            @Field(name = "resultJson", type = "very-long"),
            @Field(name = "createdDate", type = "date-time"),
            @Field(name = "tenantId", type = "id", description = "SCIPIO: 4.0.0: Pooled runtime: store of the call (empty on the base delegator); rows stay attributable after an export (G5).")
        },
        primaryKeys = { @PrimaryKey(field = "auditId") },
        indexes = {
            @Index(name = "MCP_AUDIT_TOKEN_IDX", fields = { @IndexField(name = "tokenId"), @IndexField(name = "createdDate") }),
            @Index(name = "MCP_AUDIT_IDEM_IDX", fields = { @IndexField(name = "tokenId"), @IndexField(name = "idempotencyKey") })
        }
    )
    public interface McpAuditLogEntity {}

    /**
     * Call counters per server and tool; used to rank commonly used services.
     */
    @Entity(
        name = "McpToolUsage",
        packageName = "com.ilscipio.scipio.mcp",
        title = "MCP Tool Usage",
        fields = {
            @Field(name = "serverName", type = "name"),
            @Field(name = "toolName", type = "name"),
            @Field(name = "callCount", type = "numeric"),
            @Field(name = "errorCount", type = "numeric"),
            @Field(name = "lastCallDate", type = "date-time")
        },
        primaryKeys = { @PrimaryKey(field = "serverName"), @PrimaryKey(field = "toolName") }
    )
    public interface McpToolUsageEntity {}
}

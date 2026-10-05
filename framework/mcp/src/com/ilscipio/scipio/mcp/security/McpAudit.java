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
package com.ilscipio.scipio.mcp.security;

import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.transaction.TransactionUtil;
import org.ofbiz.entity.util.EntityQuery;

import com.ilscipio.scipio.mcp.protocol.JsonRpc;

/**
 * SCIPIO: 4.0.0: Writes McpAuditLog rows in their own transaction and supports idempotent replay lookups.
 */
public final class McpAudit {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    public static final String AUDIT_MODULE = "mcp.audit";

    public static final String OK = "OK";
    public static final String ERROR = "ERROR";
    public static final String DENIED = "DENIED";
    /** A repeated call answered from the stored result of an earlier call with the same idempotency key. */
    public static final String REPLAY = "REPLAY";

    private McpAudit() {}

    public static final class Entry {
        public String tokenId;
        public String userLoginId;
        public String webappName;
        public String serverName;
        public String method;
        public String toolName;
        public Object args;
        public String status;
        public String errorMessage;
        public long durationMs;
        public String remoteAddr;
        public String requestId;
        public String idempotencyKey;
        public Object result;
    }

    public static void record(Delegator delegator, McpRedactor redactor, Entry e) {
        if (!McpConfig.isAuditEnabled()) return;
        String argsSummary = e.args != null ? McpRedactor.truncate(JsonRpc.write(redactor.redact(e.args)), McpConfig.getAuditArgsMaxChars()) : null;
        String resultJson = null;
        if (e.idempotencyKey != null && e.result != null && OK.equals(e.status)) {
            resultJson = JsonRpc.write(e.result);
            if (resultJson.length() > McpConfig.getAuditResultMaxChars()) {
                Debug.logWarning("[MCP] result for idempotencyKey=" + e.idempotencyKey + " exceeds mcp.audit.resultMaxChars; not stored, replay disabled", AUDIT_MODULE);
                resultJson = null;
            }
        }
        String tenantId = delegator.getDelegatorTenantId(); // SCIPIO: 4.0.0: pooled runtime: store of the row (G5)
        Debug.logInfo("[MCP] " + (tenantId != null ? "tenant=" + tenantId + " " : "") + "token=" + e.tokenId + " user=" + e.userLoginId + " webapp=" + e.webappName + " server=" + e.serverName
                + " tool=" + e.toolName + " status=" + e.status + " ms=" + e.durationMs + " addr=" + e.remoteAddr
                + (e.errorMessage != null ? " error=" + McpRedactor.truncate(e.errorMessage, 300) : ""), AUDIT_MODULE);
        boolean began = false;
        try {
            began = TransactionUtil.begin();
            GenericValue gv = delegator.makeValue("McpAuditLog");
            gv.set("auditId", delegator.getNextSeqId("McpAuditLog"));
            gv.set("tokenId", e.tokenId);
            gv.set("userLoginId", e.userLoginId);
            gv.set("webappName", e.webappName);
            gv.set("serverName", e.serverName);
            gv.set("method", e.method);
            gv.set("toolName", e.toolName);
            gv.set("argsSummary", argsSummary);
            gv.set("status", e.status);
            gv.set("errorMessage", e.errorMessage != null ? McpRedactor.truncate(e.errorMessage, 4000) : null);
            gv.set("durationMs", e.durationMs);
            gv.set("remoteAddr", e.remoteAddr);
            gv.set("requestId", e.requestId);
            gv.set("idempotencyKey", e.idempotencyKey);
            gv.set("resultJson", resultJson);
            gv.set("createdDate", UtilDateTime.nowTimestamp());
            gv.set("tenantId", tenantId);
            gv.create();
            TransactionUtil.commit(began);
        } catch (GenericEntityException ex) {
            try {
                TransactionUtil.rollback(began, "MCP audit write failed", ex);
            } catch (GenericEntityException ignored) {
                // nothing else to do
            }
            Debug.logError(ex, "MCP: could not write audit row", module);
        }
    }

    /** Returns the stored result of an earlier successful call with the same token and idempotency key, or null. */
    @SuppressWarnings("unchecked")
    public static Map<String, Object> findIdempotentResult(Delegator delegator, String tokenId, String idempotencyKey) {
        if (tokenId == null || idempotencyKey == null || idempotencyKey.isEmpty()) return null;
        try {
            GenericValue gv = EntityQuery.use(delegator).from("McpAuditLog")
                    .where("tokenId", tokenId, "idempotencyKey", idempotencyKey, "status", OK)
                    .orderBy("-createdDate").queryFirst();
            if (gv == null || gv.getString("resultJson") == null) return null;
            Object parsed = JsonRpc.read(gv.getString("resultJson"));
            return parsed instanceof Map ? (Map<String, Object>) parsed : null;
        } catch (GenericEntityException e) {
            Debug.logWarning(e, "MCP: idempotency lookup failed", module);
            return null;
        }
    }
}

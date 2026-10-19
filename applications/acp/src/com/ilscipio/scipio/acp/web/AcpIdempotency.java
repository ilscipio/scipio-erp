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
package com.ilscipio.scipio.acp.web;

import java.sql.Timestamp;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityQuery;

import com.ilscipio.scipio.acp.AcpConfig;
import com.ilscipio.scipio.acp.AcpException;

/**
 * SCIPIO: 4.0.0: Idempotency-Key handling of ACP POST calls, per token. The first call writes a RUNNING row; the
 * answer replaces it (DONE). A repeat with the same request gets the stored answer, a repeat with another request
 * gets 422 idempotency_conflict, a repeat while the first runs gets 409 idempotency_in_flight. A 5xx answer
 * removes the row so that the agent can retry.
 */
public final class AcpIdempotency {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    public static final String ENTITY = "AcpIdempotencyKey";

    private AcpIdempotency() {}

    /** A stored answer. */
    public static final class Stored {
        public final int status;
        public final String body;

        Stored(int status, String body) {
            this.status = status;
            this.body = body;
        }
    }

    /** Null when the call may run (a RUNNING row is written), the stored answer for a replay, or throws 409/422. */
    public static Stored begin(Delegator delegator, String tokenId, String key, String requestHash) throws AcpException {
        if (key.length() > 255) {
            throw AcpException.invalid("invalid", "Idempotency-Key is longer than 255 characters.", "Idempotency-Key");
        }
        try {
            GenericValue row = EntityQuery.use(delegator).from(ENTITY).where("tokenId", tokenId, "idempotencyKey", key).queryOne();
            if (row == null) {
                try {
                    delegator.create(ENTITY, UtilMisc.toMap("tokenId", tokenId, "idempotencyKey", key, "requestSha", requestHash,
                            "state", "RUNNING", "createdDate", UtilDateTime.nowTimestamp()));
                    return null;
                } catch (GenericEntityException e) {
                    // a parallel call with the same key won the insert
                    row = EntityQuery.use(delegator).from(ENTITY).where("tokenId", tokenId, "idempotencyKey", key).queryOne();
                    if (row == null) throw e;
                }
            }
            if (!requestHash.equals(row.getString("requestSha"))) {
                throw new AcpException(422, AcpException.INVALID_REQUEST, "idempotency_conflict",
                        "The Idempotency-Key was used with another request.", "Idempotency-Key");
            }
            if (!"DONE".equals(row.getString("state"))) {
                throw AcpException.conflict("idempotency_in_flight", "A request with this Idempotency-Key is still running.");
            }
            return new Stored(row.getLong("statusCode").intValue(), row.getString("responseBody"));
        } catch (GenericEntityException e) {
            Debug.logError(e, "ACP: idempotency lookup failed: " + e.getMessage(), module);
            throw AcpException.processing("The request could not be checked.");
        }
    }

    public static void finish(Delegator delegator, String tokenId, String key, int status, String body) {
        try {
            if (status >= 500 || status == 409) { // a temporary answer must not be replayed
                delegator.removeByAnd(ENTITY, UtilMisc.toMap("tokenId", tokenId, "idempotencyKey", key));
                return;
            }
            GenericValue row = EntityQuery.use(delegator).from(ENTITY).where("tokenId", tokenId, "idempotencyKey", key).queryOne();
            if (row != null) {
                row.set("state", "DONE");
                row.set("statusCode", (long) status);
                row.set("responseBody", body);
                row.store();
            }
        } catch (GenericEntityException e) {
            Debug.logError(e, "ACP: idempotency answer not stored: " + e.getMessage(), module);
        }
    }

    /** Removes keys older than idempotency.ttlMinutes. */
    public static int purge(Delegator delegator) throws GenericEntityException {
        Timestamp before = new Timestamp(System.currentTimeMillis() - AcpConfig.getInt(delegator, "idempotency.ttlMinutes", 1440) * 60_000L);
        return delegator.removeByCondition(ENTITY, EntityCondition.makeCondition("createdDate", EntityOperator.LESS_THAN, before));
    }
}

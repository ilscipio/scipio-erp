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
package com.ilscipio.scipio.product.store;

import java.sql.Timestamp;
import java.time.Instant;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.mcp.security.McpAudit;
import com.ilscipio.scipio.mcp.security.McpRedactor;

/**
 * The services of the store pause (R4-15, J5-01); definitions in {@code StorePauseServices}. Each write locks the row of
 * {@code ProductStore} first, so two writes of one store run one after the other: one check and one set.
 *
 * <p>SCIPIO: 4.0.0: Added (R4-15).</p>
 */
public final class StorePauseServiceImpl {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private StorePauseServiceImpl() {
    }

    public static Map<String, Object> pause(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        Instant now = Instant.now();
        try {
            String productStoreId = lockStore(delegator, (String) context.get("productStoreId"));
            Timestamp until = StorePauses.untilDate(StorePauses.instant((String) context.get("untilDate")), now);
            String note = StorePauses.note((String) context.get("note"));
            GenericValue row = StorePauses.row(delegator, productStoreId);
            if (StorePauses.isPaused(row, now)) {
                return result("ALREADY_PAUSED", row, now);
            }
            if (row == null) {
                row = delegator.makeValue(StorePauses.ENTITY, UtilMisc.toMap("productStoreId", productStoreId));
            }
            // a row of a resume, or a row whose back-on day passed (the job did not resume it yet), is a live store: the pause replaces it
            row.set("pausedDate", new Timestamp(now.toEpochMilli()));
            row.set("pauseUntilDate", until);
            row.set("pauseNote", note);
            GenericValue userLogin = (GenericValue) context.get("userLogin");
            row.set("pausedByUserLoginId", userLogin == null ? null : userLogin.getString("userLoginId"));
            nextSeq(row); // the channel reconcile of this write is pending until a reconcile finishes (review C2)
            delegator.createOrStore(row);
            // the job of the back-on day: a store that exists already gets it at its first pause (idempotent)
            StorePauses.ensureJob(delegator, "PSRES", "Store pause: back-on day", "resumeDueProductStores", "MINUTELY", 5);
            return result("PAUSED", row, now);
        } catch (IllegalArgumentException e) {
            return ServiceUtil.returnError(e.getMessage());
        } catch (GenericEntityException e) {
            Debug.logError(e, "Could not pause the store", module);
            return ServiceUtil.returnError("Could not pause the store: " + e.getMessage());
        }
    }

    public static Map<String, Object> resume(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        Instant now = Instant.now();
        try {
            String productStoreId = lockStore(delegator, (String) context.get("productStoreId"));
            GenericValue row = StorePauses.row(delegator, productStoreId);
            boolean paused = StorePauses.isPaused(row, now);
            if (row != null && row.get("pausedDate") != null) {
                // also a row whose back-on day passed: the store is live already. The row stays for the reconcile counters.
                row.set("pausedDate", null);
                row.set("pauseUntilDate", null);
                row.set("pauseNote", null);
                nextSeq(row);
                row.store();
            }
            Map<String, Object> out = ServiceUtil.returnSuccess();
            out.put("outcome", paused ? "RESUMED" : "NOT_PAUSED");
            out.putAll(StorePauses.state(row, now));
            return out;
        } catch (IllegalArgumentException e) {
            return ServiceUtil.returnError(e.getMessage());
        } catch (GenericEntityException e) {
            Debug.logError(e, "Could not resume the store", module);
            return ServiceUtil.returnError("Could not resume the store: " + e.getMessage());
        }
    }

    public static Map<String, Object> state(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        try {
            String productStoreId = storeId(delegator, (String) context.get("productStoreId"));
            Map<String, Object> out = ServiceUtil.returnSuccess();
            out.putAll(StorePauses.state(StorePauses.row(delegator, productStoreId), Instant.now()));
            return out;
        } catch (IllegalArgumentException e) {
            return ServiceUtil.returnError(e.getMessage());
        } catch (GenericEntityException e) {
            return ServiceUtil.returnError("Could not read the pause of the store: " + e.getMessage());
        }
    }

    /** The job: each due pause ends through resumeProductStore in its own transaction; an McpAuditLog row names the job. */
    public static Map<String, Object> resumeDue(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        int resumed = 0;
        try {
            List<GenericValue> due = EntityQuery.use(delegator).from(StorePauses.ENTITY)
                    .where(EntityCondition.makeCondition("pauseUntilDate", EntityOperator.LESS_THAN_EQUAL_TO, new Timestamp(System.currentTimeMillis())))
                    .queryList();
            for (GenericValue row : due) {
                String productStoreId = row.getString("productStoreId");
                long start = System.currentTimeMillis();
                Map<String, Object> res = dctx.getDispatcher().runSync("resumeProductStore",
                        UtilMisc.toMap("productStoreId", productStoreId, "userLogin", context.get("userLogin")), 60, true);
                boolean ok = ServiceUtil.isSuccess(res);
                if (ok && "RESUMED".equals(res.get("outcome"))) {
                    resumed++;
                }
                McpAudit.Entry e = new McpAudit.Entry();
                GenericValue userLogin = (GenericValue) context.get("userLogin");
                e.userLoginId = userLogin == null ? null : userLogin.getString("userLoginId");
                e.serverName = "catalog";
                e.method = "job";
                e.toolName = "store:resume";
                Map<String, Object> args = new LinkedHashMap<>();
                args.put("productStoreId", productStoreId);
                args.put("reason", "back-on day");
                e.args = args;
                e.status = ok ? McpAudit.OK : McpAudit.ERROR;
                e.errorMessage = ok ? null : ServiceUtil.getErrorMessage(res);
                e.durationMs = System.currentTimeMillis() - start;
                McpAudit.record(delegator, McpRedactor.fromConfig(), e);
            }
        } catch (GenericEntityException | GenericServiceException e) {
            Debug.logError(e, "Could not resume the due stores", module);
            return ServiceUtil.returnError("Could not resume the due stores: " + e.getMessage());
        }
        Map<String, Object> out = ServiceUtil.returnSuccess();
        out.put("resumed", resumed);
        return out;
    }

    /** The product store of the call: the given id, else the only store. */
    static String storeId(Delegator delegator, String productStoreId) throws GenericEntityException {
        if (UtilValidate.isNotEmpty(productStoreId)) {
            return productStoreId;
        }
        List<GenericValue> stores = EntityQuery.use(delegator).from("ProductStore").select("productStoreId").orderBy("productStoreId").maxRows(2).queryList();
        if (stores.size() != 1) {
            throw new IllegalArgumentException(stores.isEmpty() ? "The tenant has no product store" : "Send productStoreId: the tenant has more than one product store");
        }
        return stores.get(0).getString("productStoreId");
    }

    /**
     * Locks the row of ProductStore until the commit, so two writes of one store run one after the other. The entity engine has
     * no select for update: an update of the row to its own key takes the row lock (it changes only lastUpdatedStamp).
     */
    private static String lockStore(Delegator delegator, String productStoreId) throws GenericEntityException {
        String id = storeId(delegator, productStoreId);
        int rows = delegator.storeByCondition("ProductStore", UtilMisc.toMap("productStoreId", id), EntityCondition.makeCondition("productStoreId", id));
        if (rows == 0) {
            throw new IllegalArgumentException("Unknown product store " + id);
        }
        return id;
    }

    /** Each pause and resume counts up stateSeq; the reconcile of the channels clears the pending state only up to its own seq. */
    private static void nextSeq(GenericValue row) {
        Long seq = row.getLong("stateSeq");
        row.set("stateSeq", Long.valueOf(seq == null ? 1L : seq + 1L));
    }

    private static Map<String, Object> result(String outcome, GenericValue row, Instant now) {
        Map<String, Object> out = ServiceUtil.returnSuccess();
        out.put("outcome", outcome);
        out.putAll(StorePauses.state(row, now));
        return out;
    }
}

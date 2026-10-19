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
import java.util.Map;

import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;

/**
 * The pause of a store (R4-15, J5-01): the one state and the one rule. The row {@code ProductStorePause} exists while the seller
 * pauses the store. Each reader uses {@link #isPaused(Timestamp, Timestamp, Instant)}: the storefront, the checkout guard, the
 * quantity of each sales channel (channel-core) and the read tool of the desk. So the back-on day ends the pause for each reader
 * at the same time.
 *
 * <p>A decision reads the row without the entity cache, in the transaction of the decision.</p>
 *
 * <p>SCIPIO: 4.0.0: Added (R4-15).</p>
 */
public final class StorePauses {
    public static final String ENTITY = "ProductStorePause";
    /** The longest note for the buyers. */
    public static final int NOTE_MAX = 300;

    /** A running job older than this counts as crashed (review R4-15 C6): ensureJob registers a new chain. */
    public static final java.time.Duration RUNNING_LIMIT = java.time.Duration.ofHours(2);

    private StorePauses() {
    }

    /**
     * Registers a recurring job of the tenant when it has no open one (R4-15). The pause and the resume call it, so each store
     * that exists already gets the jobs of the pause (back-on day, hourly sweep) at its first pause, in its own database: no seed
     * data and no framework hook are needed.
     *
     * <p>Open: pending or queued, or running and started less than {@link #RUNNING_LIMIT} ago (a crashed run that stays
     * SERVICE_RUNNING does not count). One winner (review C5): the rows get the id {@link #jobId} of the hour, in a new
     * transaction, so two pauses at once (also of two stores of one tenant) create the rows once; the second create fails on the
     * key and counts as "registered by the other one".</p>
     *
     * @param frequency the RecurrenceRule frequency, for example MINUTELY or HOURLY
     * @return true when this call registered the job
     */
    public static boolean ensureJob(Delegator delegator, String prefix, String jobName, String serviceName, String frequency, int interval)
            throws GenericEntityException {
        Instant now = Instant.now();
        org.ofbiz.entity.condition.EntityCondition open = org.ofbiz.entity.condition.EntityCondition.makeCondition(
                org.ofbiz.entity.condition.EntityCondition.makeCondition("serviceName", serviceName),
                org.ofbiz.entity.condition.EntityCondition.makeCondition(java.util.Arrays.asList(
                        org.ofbiz.entity.condition.EntityCondition.makeCondition("statusId", org.ofbiz.entity.condition.EntityOperator.IN,
                                java.util.Arrays.asList("SERVICE_PENDING", "SERVICE_QUEUED")),
                        org.ofbiz.entity.condition.EntityCondition.makeCondition(
                                org.ofbiz.entity.condition.EntityCondition.makeCondition("statusId", "SERVICE_RUNNING"),
                                org.ofbiz.entity.condition.EntityCondition.makeCondition("startDateTime", org.ofbiz.entity.condition.EntityOperator.GREATER_THAN,
                                        Timestamp.from(now.minus(RUNNING_LIMIT))))),
                        org.ofbiz.entity.condition.EntityOperator.OR));
        if (EntityQuery.use(delegator).from("JobSandbox").where(open).queryCount() > 0) {
            return false;
        }
        String id = jobId(prefix, now);
        try {
            return org.ofbiz.entity.transaction.TransactionUtil.doNewTransaction(() -> {
                Timestamp start = Timestamp.from(now);
                delegator.create("RecurrenceRule", org.ofbiz.base.util.UtilMisc.toMap("recurrenceRuleId", id, "frequency", frequency,
                        "intervalNumber", Long.valueOf(interval), "countNumber", Long.valueOf(-1)));
                delegator.create("RecurrenceInfo", org.ofbiz.base.util.UtilMisc.toMap("recurrenceInfoId", id, "startDateTime", start,
                        "recurrenceRuleId", id, "recurrenceCount", Long.valueOf(0)));
                delegator.create("JobSandbox", org.ofbiz.base.util.UtilMisc.toMap("jobId", id, "jobName", jobName, "runTime", start,
                        "serviceName", serviceName, "poolId", "pool", "runAsUser", "system", "recurrenceInfoId", id,
                        "maxRecurrenceCount", Long.valueOf(-1), "statusId", "SERVICE_PENDING"));
                return Boolean.TRUE;
            }, "Could not register the job " + id, 0, false);
        } catch (GenericEntityException e) {
            // the other pause of this hour created the rows (the key exists): one chain stays
            if (EntityQuery.use(delegator).from("JobSandbox").where("jobId", id).queryCount() > 0) {
                return false;
            }
            throw e;
        }
    }

    /** The id of the rows of a job registered in the hour of {@code now} (UTC): two registrations of one hour share it (at most 20 characters, type id). */
    public static String jobId(String prefix, Instant now) {
        return prefix + "_" + java.time.format.DateTimeFormatter.ofPattern("yyMMddHH").withZone(java.time.ZoneOffset.UTC).format(now);
    }

    /** A pause or a resume that no finished reconcile of the channels covers yet (review C2). */
    public static boolean reconcilePending(Long stateSeq, Long reconciledSeq) {
        return stateSeq != null && (reconciledSeq == null || reconciledSeq < stateSeq);
    }

    /** The finished reconcile that started at {@code seq} covers each write up to it; a newer write stays pending. */
    public static void markReconciled(Delegator delegator, String productStoreId, long seq) throws GenericEntityException {
        delegator.storeByCondition(ENTITY, org.ofbiz.base.util.UtilMisc.toMap("reconciledSeq", Long.valueOf(seq)),
                org.ofbiz.entity.condition.EntityCondition.makeCondition(
                        org.ofbiz.entity.condition.EntityCondition.makeCondition("productStoreId", productStoreId),
                        org.ofbiz.entity.condition.EntityCondition.makeCondition(
                                org.ofbiz.entity.condition.EntityCondition.makeCondition("reconciledSeq", null),
                                org.ofbiz.entity.condition.EntityOperator.OR,
                                org.ofbiz.entity.condition.EntityCondition.makeCondition("reconciledSeq", org.ofbiz.entity.condition.EntityOperator.LESS_THAN, Long.valueOf(seq)))));
    }

    /** An instant of a caller: ISO-8601 with a zone (review D7); the form of java.sql.Timestamp only as a fallback. Empty: null. */
    public static Timestamp instant(String value) {
        if (value == null || value.trim().isEmpty()) {
            return null;
        }
        String s = value.trim();
        try {
            return Timestamp.from(Instant.parse(s));
        } catch (java.time.format.DateTimeParseException e) {
            try {
                return Timestamp.from(java.time.OffsetDateTime.parse(s).toInstant());
            } catch (java.time.format.DateTimeParseException e2) {
                return Timestamp.valueOf(s);
            }
        }
    }

    /** An instant for a caller: ISO-8601 in UTC (review D7). */
    public static String iso(Timestamp t) {
        return t == null ? null : t.toInstant().toString();
    }

    /** The one rule: the store has a pause, and the back-on day is empty or later than {@code now}. */
    public static boolean isPaused(Timestamp pausedDate, Timestamp untilDate, Instant now) {
        if (pausedDate == null) {
            return false;
        }
        return untilDate == null || untilDate.toInstant().isAfter(now);
    }

    /** The rule for a row (null: no pause). */
    public static boolean isPaused(GenericValue row, Instant now) {
        return row != null && isPaused(row.getTimestamp("pausedDate"), row.getTimestamp("pauseUntilDate"), now);
    }

    /** The row of the store, read without the cache; null when the store has no pause. */
    public static GenericValue row(Delegator delegator, String productStoreId) throws GenericEntityException {
        if (productStoreId == null || productStoreId.isEmpty()) {
            return null;
        }
        return EntityQuery.use(delegator).from(ENTITY).where("productStoreId", productStoreId).queryOne();
    }

    /**
     * True when the store is paused now. A read error is an {@link IllegalStateException}: a decision (a stock push, a checkout)
     * never guesses the state.
     */
    public static boolean isPaused(Delegator delegator, String productStoreId) {
        try {
            return isPaused(row(delegator, productStoreId), Instant.now());
        } catch (GenericEntityException e) {
            throw new IllegalStateException("Could not read the pause of the store " + productStoreId + ": " + e.getMessage(), e);
        }
    }

    /** The note for the buyers: trimmed, empty is null; longer than {@link #NOTE_MAX} is an error. */
    public static String note(String note) {
        if (note == null || note.trim().isEmpty()) {
            return null;
        }
        String t = note.trim();
        if (t.length() > NOTE_MAX) {
            throw new IllegalArgumentException("The note has more than " + NOTE_MAX + " characters");
        }
        return t;
    }

    /** The back-on day: empty, or later than {@code now}. */
    public static Timestamp untilDate(Timestamp untilDate, Instant now) {
        if (untilDate != null && !untilDate.toInstant().isAfter(now)) {
            throw new IllegalArgumentException("The back-on day must be in the future");
        }
        return untilDate;
    }

    /** The state for a reader: paused (Y or N), since, until, note. A row whose back-on day passed reads as live. */
    public static Map<String, Object> state(GenericValue row, Instant now) {
        Map<String, Object> out = new LinkedHashMap<>();
        boolean paused = isPaused(row, now);
        out.put("paused", paused ? "Y" : "N");
        out.put("pausedDate", paused ? iso(row.getTimestamp("pausedDate")) : null);
        out.put("pauseUntilDate", paused ? iso(row.getTimestamp("pauseUntilDate")) : null);
        out.put("pauseNote", paused ? row.getString("pauseNote") : null);
        // review round 2, point 8: the counter of each pause and resume; a reader keeps the answer with the higher number
        Long seq = row == null ? null : row.getLong("stateSeq");
        out.put("stateSeq", seq == null ? Long.valueOf(0L) : seq);
        return out;
    }
}

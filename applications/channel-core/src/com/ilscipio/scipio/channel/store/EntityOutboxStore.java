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
package com.ilscipio.scipio.channel.store;

import java.sql.Timestamp;
import java.time.Instant;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.datasource.GenericHelperInfo;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.jdbc.SQLProcessor;
import org.ofbiz.entity.model.ModelEntity;
import org.ofbiz.entity.util.EntityQuery;

import com.ilscipio.scipio.channel.core.OutboxEvent;
import com.ilscipio.scipio.channel.core.OutboxStore;

/**
 * {@link OutboxStore} on the entity ChannelOutboxEvent. It runs in the transaction of the caller: a write by a
 * service ECA joins the transaction of the order or of the inventory change.
 *
 * <p>SCIPIO: 4.0.0: Added (W1-07).</p>
 */
public final class EntityOutboxStore implements OutboxStore {
    private static final String ENTITY = "ChannelOutboxEvent";

    private final Delegator delegator;

    public EntityOutboxStore(Delegator delegator) {
        this.delegator = delegator;
    }

    @Override
    public String nextId() {
        return "EVT" + delegator.getNextSeqId(ENTITY);
    }

    @Override
    public boolean append(OutboxEvent e) {
        try {
            long same = EntityQuery.use(delegator).from(ENTITY).where("dedupeKey", e.dedupeKey).queryCount();
            if (same > 0) {
                return false;
            }
            GenericValue v = delegator.makeValue(ENTITY);
            v.set("eventId", e.eventId);
            v.set("eventType", e.eventType);
            v.set("payloadJson", e.payloadJson);
            v.set("dedupeKey", e.dedupeKey);
            v.set("createdDate", ts(e.createdDate));
            v.set("attempts", Long.valueOf(e.attempts));
            delegator.create(v);
            return true;
        } catch (GenericEntityException ex) {
            throw new IllegalStateException("Could not write the outbox event: " + ex.getMessage(), ex);
        }
    }

    /**
     * Locks the row of the cause (SELECT ... FOR UPDATE, in the transaction of the caller) until the transaction ends.
     * {@link #append} counts the dedupe key and then inserts. Without the lock two transactions on the same cause both count 0,
     * and the second insert hits the unique index CHNL_OUTBOX_KEY. The index error fails the hook and rolls back the cause
     * (on PostgreSQL the transaction is aborted, so the error cannot be caught). With the lock the second transaction waits
     * for the first commit, and then its count sees the event. The entity engine has no lock option, so this uses SQL
     * that PostgreSQL, H2, Derby, MySQL and Oracle all know.
     */
    public static void lockCauseRow(Delegator delegator, String entityName, String keyField, String keyValue) {
        try {
            ModelEntity me = delegator.getModelEntity(entityName);
            GenericHelperInfo info = delegator.getGroupHelperInfo(delegator.getEntityGroupName(entityName));
            String sql = "SELECT " + me.getField(keyField).getColName() + " FROM " + me.getTableName(info.getHelperBaseName())
                    + " WHERE " + me.getField(keyField).getColName() + " = ? FOR UPDATE";
            SQLProcessor sp = new SQLProcessor(delegator, info);
            try {
                sp.prepareStatement(sql);
                sp.setValue(keyValue);
                sp.executeQuery();
            } finally {
                sp.close();
            }
        } catch (GenericEntityException | java.sql.SQLException ex) {
            throw new IllegalStateException("Could not lock the cause row " + entityName + " " + keyValue + ": " + ex.getMessage(), ex);
        }
    }

    @Override
    public Optional<OutboxEvent> event(String eventId) {
        try {
            GenericValue v = EntityQuery.use(delegator).from(ENTITY).where("eventId", eventId).queryOne();
            return v == null ? Optional.empty() : Optional.of(toEvent(v));
        } catch (GenericEntityException ex) {
            throw new IllegalStateException("Could not read the outbox event: " + ex.getMessage(), ex);
        }
    }

    @Override
    public List<OutboxEvent> claimable(Instant now, Set<String> types, int limit) {
        List<EntityCondition> conds = new ArrayList<>();
        conds.add(EntityCondition.makeCondition("doneDate", EntityOperator.EQUALS, null));
        conds.add(EntityCondition.makeCondition("parkedDate", EntityOperator.EQUALS, null));
        conds.add(EntityCondition.makeCondition(
                EntityCondition.makeCondition("leaseUntil", EntityOperator.EQUALS, null),
                EntityOperator.OR,
                EntityCondition.makeCondition("leaseUntil", EntityOperator.LESS_THAN_EQUAL_TO, ts(now))));
        if (types != null && !types.isEmpty()) {
            conds.add(EntityCondition.makeCondition("eventType", EntityOperator.IN, types));
        }
        try {
            List<OutboxEvent> out = new ArrayList<>();
            for (GenericValue v : EntityQuery.use(delegator).from(ENTITY).where(EntityCondition.makeCondition(conds, EntityOperator.AND))
                    .orderBy("createdDate", "eventId").maxRows(limit).queryList()) {
                out.add(toEvent(v));
            }
            return out;
        } catch (GenericEntityException ex) {
            throw new IllegalStateException("Could not read the outbox: " + ex.getMessage(), ex);
        }
    }

    @Override
    public List<OutboxEvent> parked(int limit) {
        try {
            List<OutboxEvent> out = new ArrayList<>();
            for (GenericValue v : EntityQuery.use(delegator).from(ENTITY).where(EntityCondition.makeCondition(
                    EntityCondition.makeCondition("doneDate", EntityOperator.EQUALS, null),
                    EntityOperator.AND,
                    EntityCondition.makeCondition("parkedDate", EntityOperator.NOT_EQUAL, null)))
                    .orderBy("createdDate", "eventId").maxRows(limit).queryList()) {
                out.add(toEvent(v));
            }
            return out;
        } catch (GenericEntityException ex) {
            throw new IllegalStateException("Could not read the parked outbox events: " + ex.getMessage(), ex);
        }
    }

    @Override
    public boolean saveIfUnchanged(OutboxEvent e, OutboxEvent before) {
        Map<String, Object> set = new HashMap<>();
        set.put("claimedBy", e.claimedBy);
        set.put("claimedDate", ts(e.claimedDate));
        set.put("leaseUntil", ts(e.leaseUntil));
        set.put("doneDate", ts(e.doneDate));
        set.put("attempts", Long.valueOf(e.attempts));
        set.put("lastError", e.lastError);
        set.put("parkedDate", ts(e.parkedDate));
        List<EntityCondition> conds = new ArrayList<>();
        conds.add(EntityCondition.makeCondition("eventId", e.eventId));
        conds.add(EntityCondition.makeCondition("attempts", Long.valueOf(before.attempts)));
        conds.add(EntityCondition.makeCondition("doneDate", EntityOperator.EQUALS, ts(before.doneDate)));
        conds.add(EntityCondition.makeCondition("leaseUntil", EntityOperator.EQUALS, ts(before.leaseUntil)));
        conds.add(EntityCondition.makeCondition("claimedBy", EntityOperator.EQUALS, before.claimedBy));
        conds.add(EntityCondition.makeCondition("parkedDate", EntityOperator.EQUALS, ts(before.parkedDate)));
        try {
            return delegator.storeByCondition(ENTITY, set, EntityCondition.makeCondition(conds, EntityOperator.AND)) == 1;
        } catch (GenericEntityException ex) {
            throw new IllegalStateException("Could not save the outbox event: " + ex.getMessage(), ex);
        }
    }

    @Override
    public int purgeDoneBefore(Instant cutoff) {
        try {
            return delegator.removeByCondition(ENTITY, EntityCondition.makeCondition(
                    EntityCondition.makeCondition("doneDate", EntityOperator.NOT_EQUAL, null),
                    EntityOperator.AND,
                    EntityCondition.makeCondition("doneDate", EntityOperator.LESS_THAN, ts(cutoff))));
        } catch (GenericEntityException ex) {
            throw new IllegalStateException("Could not purge the outbox: " + ex.getMessage(), ex);
        }
    }

    private static OutboxEvent toEvent(GenericValue v) {
        OutboxEvent e = new OutboxEvent(v.getString("eventId"), v.getString("eventType"), v.getString("payloadJson"),
                v.getString("dedupeKey"), inst(v.getTimestamp("createdDate")));
        e.claimedBy = v.getString("claimedBy");
        e.claimedDate = inst(v.getTimestamp("claimedDate"));
        e.leaseUntil = inst(v.getTimestamp("leaseUntil"));
        e.doneDate = inst(v.getTimestamp("doneDate"));
        e.attempts = v.getLong("attempts") == null ? 0 : v.getLong("attempts").intValue();
        e.lastError = v.getString("lastError");
        e.parkedDate = inst(v.getTimestamp("parkedDate"));
        return e;
    }

    private static Timestamp ts(Instant i) {
        return i == null ? null : Timestamp.from(i);
    }

    private static Instant inst(Timestamp t) {
        return t == null ? null : t.toInstant();
    }
}

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

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.List;
import java.util.Optional;

import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityQuery;

import com.ilscipio.scipio.channel.core.ChannelSetting;
import com.ilscipio.scipio.channel.core.ChannelStore;
import com.ilscipio.scipio.channel.core.Listing;
import com.ilscipio.scipio.channel.core.ListingState;
import com.ilscipio.scipio.channel.core.OrderIntake;
import com.ilscipio.scipio.channel.core.OrderRef;
import com.ilscipio.scipio.channel.core.StockRule;
import com.ilscipio.scipio.channel.core.SyncTask;

/**
 * {@link ChannelStore} on the entities of the store (the delegator of the thread's store, so each store has its own rows).
 *
 * <p>SCIPIO: 4.0.0: Added (W1-08).</p>
 */
public final class EntityChannelStore implements ChannelStore {
    private final Delegator delegator;

    public EntityChannelStore(Delegator delegator) {
        this.delegator = delegator;
    }

    @Override
    public String nextId(String prefix) {
        return prefix + delegator.getNextSeqId("STK".equals(prefix) ? "ChannelSyncTask" : "ChannelOrderRef");
    }

    // ---- settings and rules ----

    @Override
    public List<ChannelSetting> activeSettings() {
        List<ChannelSetting> out = new ArrayList<>();
        for (GenericValue v : list("ChannelSetting", EntityCondition.makeCondition("active", "Y"))) {
            out.add(toSetting(v));
        }
        return out;
    }

    @Override
    public List<ChannelSetting> allSettings() {
        List<ChannelSetting> out = new ArrayList<>();
        try {
            for (GenericValue v : EntityQuery.use(delegator).from("ChannelSetting").queryList()) {
                out.add(toSetting(v));
            }
        } catch (GenericEntityException e) {
            throw new IllegalStateException("Could not read ChannelSetting: " + e.getMessage(), e);
        }
        return out;
    }

    @Override
    public Optional<ChannelSetting> setting(String channelId) {
        GenericValue v = one("ChannelSetting", "channelId", channelId);
        return v == null ? Optional.empty() : Optional.of(toSetting(v));
    }

    @Override
    public void saveSetting(ChannelSetting s) {
        GenericValue v = delegator.makeValue("ChannelSetting");
        v.set("channelId", s.channelId);
        v.set("connectorId", s.connectorId);
        v.set("marketplaceId", s.marketplaceId);
        v.set("productStoreId", s.productStoreId);
        v.set("accountId", s.accountId);
        v.set("currencyUomId", s.currencyUomId);
        v.set("pricesIncludeTax", s.pricesIncludeTax ? "Y" : "N");
        v.set("salesChannelEnumId", s.salesChannelEnumId);
        v.set("retentionDays", s.retentionDays == null ? null : Long.valueOf(s.retentionDays));
        v.set("retentionFrom", s.retentionFrom.name());
        v.set("active", s.active ? "Y" : "N");
        store(v);
    }

    private static ChannelSetting toSetting(GenericValue v) {
        Long days = v.getLong("retentionDays");
        String from = v.getString("retentionFrom");
        return new ChannelSetting(v.getString("connectorId"), v.getString("marketplaceId"), v.getString("productStoreId"),
                v.getString("accountId"), v.getString("currencyUomId"), "Y".equals(v.getString("pricesIncludeTax")),
                v.getString("salesChannelEnumId"), days == null ? null : Integer.valueOf(days.intValue()),
                from == null ? null : ChannelSetting.RetentionFrom.valueOf(from), !"N".equals(v.getString("active")));
    }

    @Override
    public StockRule stockRule(String channelId) {
        GenericValue v = one("ChannelStockRule", "channelId", channelId);
        if (v == null) {
            return StockRule.NONE;
        }
        Long buffer = v.getLong("buffer");
        Long max = v.getLong("maxQuantity");
        return new StockRule(buffer == null ? 0 : buffer.intValue(), max == null ? null : Integer.valueOf(max.intValue()));
    }

    @Override
    public void saveStockRule(String channelId, StockRule rule) {
        GenericValue v = delegator.makeValue("ChannelStockRule");
        v.set("channelId", channelId);
        v.set("buffer", Long.valueOf(rule.buffer));
        v.set("maxQuantity", rule.maxQuantity == null ? null : Long.valueOf(rule.maxQuantity));
        store(v);
    }

    // ---- listings ----

    @Override
    public Optional<Listing> listing(String productId, String channelId) {
        GenericValue v = one("ChannelListing", "productId", productId, "channelId", channelId);
        return v == null ? Optional.empty() : Optional.of(toListing(v));
    }

    @Override
    public Optional<Listing> listingByExternalId(String channelId, String externalId) {
        List<GenericValue> rows = list("ChannelListing", EntityCondition.makeCondition(
                EntityCondition.makeCondition("channelId", channelId), EntityCondition.makeCondition("externalId", externalId)));
        return rows.isEmpty() ? Optional.empty() : Optional.of(toListing(rows.get(0)));
    }

    @Override
    public List<Listing> listingsOfProduct(String productId) {
        List<Listing> out = new ArrayList<>();
        for (GenericValue v : list("ChannelListing", EntityCondition.makeCondition("productId", productId))) {
            out.add(toListing(v));
        }
        return out;
    }

    @Override
    public void saveListing(Listing l) {
        GenericValue v = delegator.makeValue("ChannelListing");
        v.set("productId", l.productId);
        v.set("channelId", l.channelId);
        v.set("externalId", l.externalId);
        v.set("listingState", l.state.name());
        v.set("lastSyncDate", ts(l.lastSyncDate));
        v.set("errorsJson", l.errorsJson);
        v.set("fixHint", l.fixHint);
        v.set("variationGroupId", l.variationGroupId);
        v.set("variationAxesJson", l.variationAxesJson);
        v.set("lastPushedQuantity", l.lastPushedQuantity == null ? null : Long.valueOf(l.lastPushedQuantity));
        store(v);
    }

    private static Listing toListing(GenericValue v) {
        Listing l = new Listing(v.getString("productId"), v.getString("channelId"));
        l.externalId = v.getString("externalId");
        l.state = ListingState.parse(v.getString("listingState"));
        l.lastSyncDate = inst(v.getTimestamp("lastSyncDate"));
        l.errorsJson = v.getString("errorsJson");
        l.fixHint = v.getString("fixHint");
        l.variationGroupId = v.getString("variationGroupId");
        l.variationAxesJson = v.getString("variationAxesJson");
        Long q = v.getLong("lastPushedQuantity");
        l.lastPushedQuantity = q == null ? null : Integer.valueOf(q.intValue());
        return l;
    }

    @Override
    public Optional<String> productIdBySku(String sku) {
        GenericValue p = one("Product", "productId", sku);
        if (p != null) {
            return Optional.of(p.getString("productId"));
        }
        List<GenericValue> ids = list("GoodIdentification", EntityCondition.makeCondition(
                EntityCondition.makeCondition("goodIdentificationTypeId", "SKU"), EntityCondition.makeCondition("idValue", sku)));
        return ids.isEmpty() ? Optional.empty() : Optional.of(ids.get(0).getString("productId"));
    }

    // ---- sync tasks ----

    @Override
    public List<SyncTask> openTasks() {
        List<SyncTask> out = new ArrayList<>();
        for (GenericValue v : list("ChannelSyncTask", EntityCondition.makeCondition("taskState", EntityOperator.IN,
                Arrays.asList(SyncTask.State.PENDING.name(), SyncTask.State.CLAIMED.name())))) {
            out.add(toTask(v));
        }
        return out;
    }

    @Override
    public List<SyncTask> openTasksOf(String channelId, String productId) {
        List<SyncTask> out = new ArrayList<>();
        for (GenericValue v : list("ChannelSyncTask", EntityCondition.makeCondition(
                EntityCondition.makeCondition("channelId", channelId), EntityCondition.makeCondition("productId", productId),
                EntityCondition.makeCondition("taskState", EntityOperator.IN,
                        Arrays.asList(SyncTask.State.PENDING.name(), SyncTask.State.CLAIMED.name()))))) {
            out.add(toTask(v));
        }
        return out;
    }

    @Override
    public boolean saveTaskIfUnchanged(SyncTask t, SyncTask before) {
        Map<String, Object> set = new HashMap<>();
        set.put("externalId", t.externalId);
        set.put("quantity", Long.valueOf(t.quantity));
        set.put("taskState", t.state.name());
        set.put("attempts", Long.valueOf(t.attempts));
        set.put("dueDate", ts(t.dueDate));
        set.put("leaseUntil", ts(t.leaseUntil));
        set.put("lastError", t.lastError == null ? null : (t.lastError.length() > 255 ? t.lastError.substring(0, 255) : t.lastError));
        set.put("doneDate", ts(t.doneDate));
        EntityCondition cond = EntityCondition.makeCondition(
                EntityCondition.makeCondition("taskId", t.taskId),
                EntityCondition.makeCondition("taskState", before.state.name()),
                EntityCondition.makeCondition("attempts", Long.valueOf(before.attempts)),
                EntityCondition.makeCondition("quantity", Long.valueOf(before.quantity)),
                EntityCondition.makeCondition("leaseUntil", EntityOperator.EQUALS, ts(before.leaseUntil)));
        try {
            return delegator.storeByCondition("ChannelSyncTask", set, cond) == 1;
        } catch (GenericEntityException e) {
            throw new IllegalStateException("Could not save ChannelSyncTask: " + e.getMessage(), e);
        }
    }

    @Override
    public Optional<SyncTask> task(String taskId) {
        GenericValue v = one("ChannelSyncTask", "taskId", taskId);
        return v == null ? Optional.empty() : Optional.of(toTask(v));
    }

    @Override
    public void saveTask(SyncTask t) {
        GenericValue v = delegator.makeValue("ChannelSyncTask");
        v.set("taskId", t.taskId);
        v.set("channelId", t.channelId);
        v.set("productId", t.productId);
        v.set("externalId", t.externalId);
        v.set("quantity", Long.valueOf(t.quantity));
        v.set("taskState", t.state.name());
        v.set("attempts", Long.valueOf(t.attempts));
        v.set("dueDate", ts(t.dueDate));
        v.set("leaseUntil", ts(t.leaseUntil));
        v.set("lastError", t.lastError == null ? null : (t.lastError.length() > 255 ? t.lastError.substring(0, 255) : t.lastError));
        v.set("createdDate", ts(t.createdDate));
        v.set("doneDate", ts(t.doneDate));
        store(v);
    }

    private static SyncTask toTask(GenericValue v) {
        SyncTask t = new SyncTask(v.getString("taskId"), v.getString("channelId"), v.getString("productId"),
                v.getString("externalId"), v.getLong("quantity") == null ? 0 : v.getLong("quantity").intValue(),
                inst(v.getTimestamp("createdDate")));
        t.state = SyncTask.State.valueOf(v.getString("taskState"));
        t.attempts = v.getLong("attempts") == null ? 0 : v.getLong("attempts").intValue();
        t.dueDate = inst(v.getTimestamp("dueDate"));
        t.leaseUntil = inst(v.getTimestamp("leaseUntil"));
        t.lastError = v.getString("lastError");
        t.doneDate = inst(v.getTimestamp("doneDate"));
        return t;
    }

    // ---- order refs ----

    @Override
    public Optional<OrderRef> orderRef(String channelId, String externalOrderId) {
        List<GenericValue> rows = list("ChannelOrderRef", EntityCondition.makeCondition(
                EntityCondition.makeCondition("channelId", channelId),
                EntityCondition.makeCondition("externalOrderId", externalOrderId)));
        return rows.isEmpty() ? Optional.empty() : Optional.of(toRef(rows.get(0)));
    }

    @Override
    public Optional<OrderRef> orderRefByOrderId(String orderId) {
        GenericValue v = one("ChannelOrderRef", "orderId", orderId);
        return v == null ? Optional.empty() : Optional.of(toRef(v));
    }

    @Override
    public void saveOrderRef(OrderRef r) {
        GenericValue v = delegator.makeValue("ChannelOrderRef");
        v.set("orderId", r.orderId);
        v.set("channelId", r.channelId);
        v.set("externalOrderId", r.externalOrderId);
        v.set("payoutId", r.payoutId);
        v.set("feesAmount", r.feesAmount);
        v.set("placedDate", ts(r.placedDate));
        v.set("buyerExternalId", r.buyerExternalId);
        v.set("closedDate", ts(r.closedDate));
        v.set("buyerDataDueDate", ts(r.buyerDataDueDate));
        v.set("buyerDataErasedDate", ts(r.buyerDataErasedDate));
        store(v);
    }

    @Override
    public String reserveOrderRef(String channelId, String externalOrderId) {
        if (orderRef(channelId, externalOrderId).isPresent()) {
            return null;
        }
        String id = nextId(OrderIntake.PLACEHOLDER_PREFIX);
        try {
            GenericValue v = delegator.makeValue("ChannelOrderRef");
            v.set("orderId", id);
            v.set("channelId", channelId);
            v.set("externalOrderId", externalOrderId);
            delegator.create(v); // the unique index on channel and external order id stops a parallel call
            return id;
        } catch (GenericEntityException e) {
            return null;
        }
    }

    @Override
    public void releaseOrderRef(String placeholderId) {
        try {
            delegator.removeByAnd("ChannelOrderRef", "orderId", placeholderId);
        } catch (GenericEntityException e) {
            throw new IllegalStateException("Could not release the order ref: " + e.getMessage(), e);
        }
    }

    @Override
    public void replaceOrderRef(String placeholderId, OrderRef ref) {
        releaseOrderRef(placeholderId);
        saveOrderRef(ref);
    }

    @Override
    public List<OrderRef> orderRefsDueForErasure(Instant now) {
        List<OrderRef> out = new ArrayList<>();
        for (GenericValue v : list("ChannelOrderRef", EntityCondition.makeCondition(
                EntityCondition.makeCondition("buyerDataDueDate", EntityOperator.LESS_THAN_EQUAL_TO, ts(now)),
                EntityCondition.makeCondition("buyerDataErasedDate", EntityOperator.EQUALS, null)))) {
            out.add(toRef(v));
        }
        return out;
    }

    @Override
    public List<OrderRef> orderRefsOfBuyer(List<String> channelIds, String buyerExternalId) {
        List<OrderRef> out = new ArrayList<>();
        if (channelIds.isEmpty()) {
            return out;
        }
        for (GenericValue v : list("ChannelOrderRef", EntityCondition.makeCondition(
                EntityCondition.makeCondition("channelId", EntityOperator.IN, channelIds),
                EntityCondition.makeCondition("buyerExternalId", buyerExternalId),
                EntityCondition.makeCondition("buyerDataErasedDate", EntityOperator.EQUALS, null)))) {
            out.add(toRef(v));
        }
        return out;
    }

    private static OrderRef toRef(GenericValue v) {
        OrderRef r = new OrderRef(v.getString("orderId"), v.getString("channelId"), v.getString("externalOrderId"));
        r.payoutId = v.getString("payoutId");
        r.feesAmount = (BigDecimal) v.get("feesAmount");
        r.placedDate = inst(v.getTimestamp("placedDate"));
        r.buyerExternalId = v.getString("buyerExternalId");
        r.closedDate = inst(v.getTimestamp("closedDate"));
        r.buyerDataDueDate = inst(v.getTimestamp("buyerDataDueDate"));
        r.buyerDataErasedDate = inst(v.getTimestamp("buyerDataErasedDate"));
        return r;
    }

    // ---- helpers ----

    private GenericValue one(String entity, Object... keyValues) {
        try {
            return EntityQuery.use(delegator).from(entity).where(keyValues).queryFirst();
        } catch (GenericEntityException e) {
            throw new IllegalStateException("Could not read " + entity + ": " + e.getMessage(), e);
        }
    }

    private List<GenericValue> list(String entity, EntityCondition cond) {
        try {
            return EntityQuery.use(delegator).from(entity).where(cond).queryList();
        } catch (GenericEntityException e) {
            throw new IllegalStateException("Could not read " + entity + ": " + e.getMessage(), e);
        }
    }

    private void store(GenericValue v) {
        try {
            delegator.createOrStore(v);
        } catch (GenericEntityException e) {
            throw new IllegalStateException("Could not save " + v.getEntityName() + ": " + e.getMessage(), e);
        }
    }

    private static Timestamp ts(Instant i) {
        return i == null ? null : Timestamp.from(i);
    }

    private static Instant inst(Timestamp t) {
        return t == null ? null : t.toInstant();
    }
}

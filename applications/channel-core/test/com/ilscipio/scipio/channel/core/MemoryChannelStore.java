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
package com.ilscipio.scipio.channel.core;

import java.time.Instant;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/** In-memory {@link ChannelStore} for the tests. It returns copies, as the entity store does. */
public class MemoryChannelStore implements ChannelStore {
    private final Map<String, ChannelSetting> settings = new LinkedHashMap<>();
    private final Map<String, StockRule> rules = new HashMap<>();
    private final Map<String, Listing> listings = new LinkedHashMap<>();
    private final Map<String, SyncTask> tasks = new LinkedHashMap<>();
    private final Map<String, OrderRef> refs = new LinkedHashMap<>();
    public final Map<String, String> productBySku = new HashMap<>();
    private int seq;

    @Override
    public String nextId(String prefix) {
        return prefix + String.format("%05d", ++seq);
    }

    @Override
    public List<ChannelSetting> activeSettings() {
        List<ChannelSetting> out = new ArrayList<>();
        for (ChannelSetting s : settings.values()) {
            if (s.active) {
                out.add(s);
            }
        }
        return out;
    }

    @Override
    public List<ChannelSetting> allSettings() {
        return new ArrayList<>(settings.values());
    }

    @Override
    public Optional<ChannelSetting> setting(String channelId) {
        return Optional.ofNullable(settings.get(channelId));
    }

    @Override
    public void saveSetting(ChannelSetting s) {
        settings.put(s.channelId, s);
    }

    @Override
    public StockRule stockRule(String channelId) {
        return rules.getOrDefault(channelId, StockRule.NONE);
    }

    @Override
    public void saveStockRule(String channelId, StockRule rule) {
        rules.put(channelId, rule);
    }

    private static String key(String productId, String channelId) {
        return channelId + "|" + productId;
    }

    @Override
    public Optional<Listing> listing(String productId, String channelId) {
        Listing l = listings.get(key(productId, channelId));
        return l == null ? Optional.empty() : Optional.of(l.copy());
    }

    @Override
    public Optional<Listing> listingByExternalId(String channelId, String externalId) {
        for (Listing l : listings.values()) {
            if (l.channelId.equals(channelId) && externalId.equals(l.externalId)) {
                return Optional.of(l.copy());
            }
        }
        return Optional.empty();
    }

    @Override
    public List<Listing> listingsOfProduct(String productId) {
        List<Listing> out = new ArrayList<>();
        for (Listing l : listings.values()) {
            if (l.productId.equals(productId)) {
                out.add(l.copy());
            }
        }
        return out;
    }

    @Override
    public void saveListing(Listing l) {
        listings.put(key(l.productId, l.channelId), l.copy());
    }

    @Override
    public Optional<String> productIdBySku(String sku) {
        return Optional.ofNullable(productBySku.get(sku));
    }

    @Override
    public List<SyncTask> openTasks() {
        List<SyncTask> out = new ArrayList<>();
        for (SyncTask t : tasks.values()) {
            if (t.isOpen()) {
                out.add(t.copy());
            }
        }
        return out;
    }

    @Override
    public List<SyncTask> openTasksOf(String channelId, String productId) {
        List<SyncTask> out = new ArrayList<>();
        for (SyncTask t : openTasks()) {
            if (t.channelId.equals(channelId) && t.productId.equals(productId)) {
                out.add(t);
            }
        }
        return out;
    }

    @Override
    public boolean saveTaskIfUnchanged(SyncTask t, SyncTask before) {
        SyncTask now = tasks.get(t.taskId);
        if (now == null || now.state != before.state || now.attempts != before.attempts || now.quantity != before.quantity
                || !java.util.Objects.equals(now.leaseUntil, before.leaseUntil)) {
            return false;
        }
        tasks.put(t.taskId, t.copy());
        return true;
    }

    /** Test hook: a task changes behind the back of the caller. */
    public void changeBehindBack(String taskId, java.util.function.Consumer<SyncTask> change) {
        SyncTask t = tasks.get(taskId);
        change.accept(t);
    }

    @Override
    public Optional<SyncTask> task(String taskId) {
        SyncTask t = tasks.get(taskId);
        return t == null ? Optional.empty() : Optional.of(t.copy());
    }

    @Override
    public void saveTask(SyncTask t) {
        tasks.put(t.taskId, t.copy());
    }

    public List<SyncTask> allTasks() {
        List<SyncTask> out = new ArrayList<>();
        for (SyncTask t : tasks.values()) {
            out.add(t.copy());
        }
        return out;
    }

    private static String refKey(String channelId, String externalOrderId) {
        return channelId + "|" + externalOrderId;
    }

    @Override
    public Optional<OrderRef> orderRef(String channelId, String externalOrderId) {
        OrderRef r = refs.get(refKey(channelId, externalOrderId));
        return r == null ? Optional.empty() : Optional.of(r.copy());
    }

    @Override
    public Optional<OrderRef> orderRefByOrderId(String orderId) {
        for (OrderRef r : refs.values()) {
            if (r.orderId.equals(orderId)) {
                return Optional.of(r.copy());
            }
        }
        return Optional.empty();
    }

    @Override
    public void saveOrderRef(OrderRef r) {
        refs.put(refKey(r.channelId, r.externalOrderId), r.copy());
    }

    @Override
    public String reserveOrderRef(String channelId, String externalOrderId) {
        if (refs.containsKey(refKey(channelId, externalOrderId))) {
            return null;
        }
        String id = nextId(OrderIntake.PLACEHOLDER_PREFIX);
        refs.put(refKey(channelId, externalOrderId), new OrderRef(id, channelId, externalOrderId));
        return id;
    }

    @Override
    public void releaseOrderRef(String placeholderId) {
        refs.values().removeIf(r -> r.orderId.equals(placeholderId));
    }

    @Override
    public void replaceOrderRef(String placeholderId, OrderRef ref) {
        releaseOrderRef(placeholderId);
        saveOrderRef(ref);
    }

    @Override
    public List<OrderRef> orderRefsDueForErasure(Instant now) {
        List<OrderRef> out = new ArrayList<>();
        for (OrderRef r : refs.values()) {
            if (r.buyerDataErasedDate == null && r.buyerDataDueDate != null && !r.buyerDataDueDate.isAfter(now)) {
                out.add(r.copy());
            }
        }
        return out;
    }

    @Override
    public List<OrderRef> orderRefsOfBuyer(List<String> channelIds, String buyerExternalId) {
        List<OrderRef> out = new ArrayList<>();
        for (OrderRef r : refs.values()) {
            if (r.buyerDataErasedDate == null && channelIds.contains(r.channelId) && buyerExternalId.equals(r.buyerExternalId)) {
                out.add(r.copy());
            }
        }
        return out;
    }
}

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
import java.util.List;
import java.util.Optional;

/**
 * Storage port of channel-core. The entity-backed implementation is {@code EntityChannelStore};
 * the tests use an in-memory one. All returned objects are copies: a change needs a save call.
 *
 * <p>SCIPIO: 4.0.0: Added (W1-08).</p>
 */
public interface ChannelStore {

    String nextId(String prefix);

    // settings and rules
    List<ChannelSetting> activeSettings();

    /** Active and inactive channels (a buyer-deletion notice reaches a channel that the seller switched off). */
    List<ChannelSetting> allSettings();

    Optional<ChannelSetting> setting(String channelId);

    void saveSetting(ChannelSetting s);

    StockRule stockRule(String channelId);

    void saveStockRule(String channelId, StockRule rule);

    // listings
    Optional<Listing> listing(String productId, String channelId);

    Optional<Listing> listingByExternalId(String channelId, String externalId);

    List<Listing> listingsOfProduct(String productId);

    void saveListing(Listing l);

    /** The store product for a SKU (the product id, or the SKU good identification), if one exists. */
    Optional<String> productIdBySku(String sku);

    // sync tasks
    /** All tasks that are pending or claimed. */
    List<SyncTask> openTasks();

    /** The open tasks of one listing. */
    List<SyncTask> openTasksOf(String channelId, String productId);

    Optional<SyncTask> task(String taskId);

    /** Saves a new task. */
    void saveTask(SyncTask t);

    /**
     * Compare and set: saves {@code t} only when the stored task still has the state, attempts, quantity and lease of
     * {@code before}. Returns false when another worker changed it.
     */
    boolean saveTaskIfUnchanged(SyncTask t, SyncTask before);

    // order refs
    Optional<OrderRef> orderRef(String channelId, String externalOrderId);

    Optional<OrderRef> orderRefByOrderId(String orderId);

    void saveOrderRef(OrderRef r);

    /**
     * Takes the right to make the store order of a channel order: writes a placeholder ref (the unique key of channel
     * and external order id is the lock). Returns the placeholder id, or null when a ref exists already.
     */
    String reserveOrderRef(String channelId, String externalOrderId);

    void releaseOrderRef(String placeholderId);

    /** Replaces the placeholder with the real ref. */
    void replaceOrderRef(String placeholderId, OrderRef ref);

    /** Refs with a due date at or before {@code now} and no erase date. */
    List<OrderRef> orderRefsDueForErasure(Instant now);

    /** Refs of the given channels for one buyer id at the channel, not yet erased. */
    List<OrderRef> orderRefsOfBuyer(List<String> channelIds, String buyerExternalId);
}

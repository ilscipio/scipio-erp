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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.math.BigDecimal;
import java.time.Duration;
import java.time.Instant;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.Test;

/** R4-15 review round 2: the fencing of the reports (point 1) and the quantity of a NONE lease (point 7). */
public class FencingTest {
    static final String STORE = "STORE_A";
    static final String CH = "ebay-us";

    final MutableClock clock = new MutableClock(Instant.parse("2026-10-10T10:00:00Z"));
    final MemoryChannelStore store = new MemoryChannelStore();
    final Map<String, Integer> atp = new HashMap<>();
    final Set<String> paused = new HashSet<>();
    final StockSource stock = new PausedStockSource((p, s) -> BigDecimal.valueOf(atp.getOrDefault(p, 0)), paused::contains);
    final StockSync sync = new StockSync(store, stock, clock);
    final SyncQueue queue = new SyncQueue(store, clock, stock);

    FencingTest() {
        store.saveSetting(new ChannelSetting("ebay", "us", STORE, "ACC_1", "USD", false, "EBAY_CHANNEL", null,
                ChannelSetting.RetentionFrom.CLOSED, true));
        atp.put("P1", 5);
        Listing l = new Listing("P1", CH);
        l.externalId = "E1";
        l.state = ListingState.LIVE;
        l.lastPushedQuantity = 5;
        store.saveListing(l);
    }

    @Test
    void aLatePauseReportNeverOverwritesTheNewerResumePush() {
        paused.add(STORE);
        sync.reconcileStore(STORE, false);
        SyncTask slow = queue.claim(10, Duration.ofSeconds(60)).get(0); // worker 1 pushes 0, slowly
        assertEquals(0, slow.quantity);
        clock.advance(Duration.ofSeconds(61)); // the lease ended
        paused.remove(STORE);
        sync.reconcileStore(STORE, false);
        List<SyncTask> fast = queue.claim(10, Duration.ofSeconds(60)); // worker 2 pushes 5
        assertEquals(1, fast.size());
        assertNotEquals(slow.taskId, fast.get(0).taskId, "a new claim after the end of a lease has a new task id");
        assertEquals(5, fast.get(0).quantity);
        queue.succeeded(fast.get(0).taskId);
        assertEquals(Integer.valueOf(5), store.listing("P1", CH).get().lastPushedQuantity);
        queue.succeeded(slow.taskId); // the late report of worker 1 (its 0 landed after the 5)
        assertNull(store.listing("P1", CH).get().lastPushedQuantity,
                "nothing recorded; the pushed quantity is forgotten, so the next plan pushes again");
        sync.reconcileStore(STORE, false);
        sync.stockChanged("P1");
        assertEquals(5, queue.claim(10, Duration.ofSeconds(60)).get(0).quantity, "the listing is pushed again with the real value");
    }

    @Test
    void aLateReportWhileAnotherTaskHoldsTheLeaseChangesNothing() {
        SyncQueue.Lease first = queue.lease(CH, "P1", Duration.ofSeconds(30));
        clock.advance(Duration.ofSeconds(31));
        SyncQueue.Lease second = queue.lease(CH, "P1", Duration.ofSeconds(120));
        assertEquals(SyncQueue.Lease.Kind.LEASED, second.kind);
        assertNotEquals(first.task.taskId, second.task.taskId);
        queue.failed(first.task.taskId, false, null, "late", null);
        Listing l = store.listing("P1", CH).get();
        assertNull(l.errorsJson, "a late failed report writes no error on the listing");
        assertEquals(SyncQueue.Lease.Kind.BUSY, queue.lease(CH, "P1", Duration.ofSeconds(120)).kind, "the lease of the second stays");
        queue.succeeded(second.task.taskId);
        assertEquals(Integer.valueOf(5), store.listing("P1", CH).get().lastPushedQuantity);
    }

    @Test
    void aReportInsideTheLeaseRecordsTheQuantity() {
        atp.put("P1", 7);
        sync.stockChanged("P1");
        SyncTask t = queue.claim(10, Duration.ofSeconds(60)).get(0);
        queue.succeeded(t.taskId);
        assertEquals(Integer.valueOf(7), store.listing("P1", CH).get().lastPushedQuantity);
        assertEquals(SyncTask.State.DONE, store.task(t.taskId).get().state);
        queue.succeeded(t.taskId); // a repeated report of a finished task
        assertEquals(Integer.valueOf(7), store.listing("P1", CH).get().lastPushedQuantity, "a repeated report keeps the pushed quantity");
    }

    @Test
    void aNoneLeaseGivesAQuantityOnlyForAPausedStoreOrANewListing() {
        Listing ended = new Listing("P2", CH);
        ended.externalId = "E2"; // exists, takes no stock: a quantity of its own (fixed, made to order)
        ended.state = ListingState.ENDED;
        store.saveListing(ended);
        atp.put("P2", 9);
        SyncQueue.Lease live = queue.lease(CH, "P2", Duration.ofSeconds(120));
        assertEquals(SyncQueue.Lease.Kind.NONE, live.kind);
        assertNull(live.quantity, "the hub keeps its own quantity");
        paused.add(STORE);
        assertEquals(Integer.valueOf(0), queue.lease(CH, "P2", Duration.ofSeconds(120)).quantity, "a paused store: 0");
        paused.remove(STORE);
        Listing fresh = new Listing("P3", CH);
        fresh.state = ListingState.PENDING;
        store.saveListing(fresh);
        atp.put("P3", 4);
        assertEquals(Integer.valueOf(4), queue.lease(CH, "P3", Duration.ofSeconds(120)).quantity, "a new listing: the state");
        assertTrue(DEFAULT_IS_LONGER_THAN_PUSH_WINDOW());
    }

    private static boolean DEFAULT_IS_LONGER_THAN_PUSH_WINDOW() {
        return SyncQueue.DEFAULT_LEASE.toMillis() > (long) (60_000 * 0.7);
    }
}

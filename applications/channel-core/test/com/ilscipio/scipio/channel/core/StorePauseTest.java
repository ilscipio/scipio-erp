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
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.math.BigDecimal;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.Test;

/**
 * R4-15 (J5-01): the store pause on the sales channels. The row of the core is the only state; the plan, the claim and the
 * lease read it through {@link PausedStockSource} at their own moment, so a late or repeated run sends the state of that moment.
 */
public class StorePauseTest {
    static final Instant T0 = Instant.parse("2026-10-10T10:00:00Z");
    static final String STORE = "STORE_A";
    static final String CH = "ebay-us";

    final MutableClock clock = new MutableClock(T0);
    final MemoryChannelStore store = new MemoryChannelStore();
    final Map<String, Integer> atp = new HashMap<>();
    final Set<String> paused = new HashSet<>();
    /** The normal rule: ATP; the product SERVICE1 is always available (its rule gives 999, it has no stock count). */
    final StockSource normal = (p, s) -> "SERVICE1".equals(p) ? BigDecimal.valueOf(999) : BigDecimal.valueOf(atp.getOrDefault(p, 0));
    final StockSource stock = new PausedStockSource(normal, paused::contains);
    final StockSync sync = new StockSync(store, stock, clock);
    final SyncQueue queue = new SyncQueue(store, clock, stock);

    StorePauseTest() {
        store.saveSetting(new ChannelSetting("ebay", "us", STORE, "ACC_1", "USD", false, "EBAY_CHANNEL", null,
                ChannelSetting.RetentionFrom.CLOSED, true));
        store.saveSetting(new ChannelSetting("ebay", "de", "STORE_B", "ACC_2", "EUR", true, "EBAY_CHANNEL", null,
                ChannelSetting.RetentionFrom.CLOSED, true));
        atp.put("P1", 10);
        atp.put("P2", 5);
        live("P1", CH, "E1", 10);
        live("P2", CH, "E2", 5);
    }

    private Listing live(String productId, String channelId, String externalId, Integer known) {
        Listing l = new Listing(productId, channelId);
        l.externalId = externalId;
        l.state = ListingState.LIVE;
        l.lastPushedQuantity = known;
        store.saveListing(l);
        return l;
    }

    private void pause() {
        paused.add(STORE);
        sync.reconcileStore(STORE, false);
    }

    private void resume() {
        paused.remove(STORE);
        sync.reconcileStore(STORE, false);
    }

    /** The hub: claims each due task and reports the push as done. */
    private Map<String, Integer> pushAll() {
        Map<String, Integer> sent = new HashMap<>();
        for (SyncTask t : queue.claim(50, Duration.ofSeconds(30))) {
            sent.put(t.productId, t.quantity);
            queue.succeeded(t.taskId);
        }
        return sent;
    }

    @Test
    void pausedSourceGivesZeroAndLiveSourceGivesTheNormalRule() {
        paused.add(STORE);
        assertEquals(BigDecimal.ZERO, stock.availableToPromise("P1", STORE));
        assertEquals(BigDecimal.ZERO, stock.availableToPromise("SERVICE1", STORE), "an always available product gets 0 too");
        assertEquals(BigDecimal.valueOf(5), stock.availableToPromise("P2", "STORE_B"), "another store runs on");
        paused.remove(STORE);
        assertEquals(BigDecimal.valueOf(10), stock.availableToPromise("P1", STORE));
    }

    @Test
    void pausePlansZeroForEachListingThatTakesStockAndResumeGivesTheRealQuantity() {
        Listing draft = new Listing("P3", CH);
        draft.state = ListingState.DRAFT;
        store.saveListing(draft);
        Listing ended = live("P4", CH, "E4", 3);
        ended.state = ListingState.ENDED;
        store.saveListing(ended);
        pause();
        Map<String, Integer> sent = pushAll();
        assertEquals(Map.of("P1", 0, "P2", 0), sent, "only the listings that take stock, each with 0");
        resume();
        assertEquals(Map.of("P1", 10, "P2", 5), pushAll(), "the resume gives back the quantity of the stock rule");
    }

    @Test
    void pauseAndResumeBeforeTheClaimSendNothing() {
        pause();
        resume();
        assertTrue(pushAll().isEmpty(), "the channel knows the real quantity already");
    }

    @Test
    void aTaskPlannedBeforeThePauseSendsZeroAtTheClaim() {
        atp.put("P1", 7);
        sync.stockChanged("P1");
        paused.add(STORE); // the pause commits; its plan comes later
        assertEquals(Map.of("P1", 0), pushAll(), "the claim reads the state of its own moment");
    }

    @Test
    void aResumeDuringALeasedPushOfZeroSendsTheRealQuantityAfterTheReport() {
        pause();
        List<SyncTask> leased = queue.claim(50, Duration.ofSeconds(30));
        assertEquals(2, leased.size());
        resume(); // the pushes of 0 are on their way
        assertTrue(queue.claim(50, Duration.ofSeconds(30)).isEmpty(), "no second task of a leased listing");
        for (SyncTask t : leased) {
            assertEquals(0, t.quantity);
            queue.succeeded(t.taskId);
        }
        assertEquals(Map.of("P1", 10, "P2", 5), pushAll(), "the newer state reaches the channel after the old push");
    }

    @Test
    void aLatePlanOfAnOlderPauseNeverSendsZeroAfterTheResume() {
        paused.add(STORE);
        paused.remove(STORE); // pause and resume committed; the plan of the pause runs late
        sync.reconcileStore(STORE, false);
        sync.reconcileStore(STORE, false); // and twice
        assertTrue(pushAll().isEmpty(), "the plan reads the state, not the intent of the pause");
    }

    @Test
    void anAlwaysAvailableProductGetsZeroAndThenItsNormalRuleNotAStoredNumber() {
        live("SERVICE1", CH, "E9", 999);
        pause();
        assertEquals(Integer.valueOf(0), pushAll().get("SERVICE1"));
        assertEquals(Integer.valueOf(0), store.listing("SERVICE1", CH).get().lastPushedQuantity);
        resume();
        assertEquals(Integer.valueOf(999), pushAll().get("SERVICE1"), "the resume asks the normal rule again (always available)");
    }

    @Test
    void theSweepSendsZeroAgainWhilePaused() {
        pause();
        pushAll();
        assertTrue(sync.reconcileStore(STORE, false).isEmpty(), "the channel knows 0");
        List<SyncTask> again = sync.reconcileStore(STORE, true);
        assertEquals(2, again.size(), "the sweep forgets the known value");
        assertEquals(Map.of("P1", 0, "P2", 0), pushAll(), "a quantity that the seller changed on the marketplace goes back to 0");
    }

    @Test
    void anOrderOnAnotherChannelDuringThePausePlansZero() {
        paused.add(STORE);
        atp.put("P1", 4);
        sync.stockChanged("P1");
        assertEquals(Map.of("P1", 0), pushAll());
    }

    @Test
    void aPublishLeaseCarriesTheQuantityOfTheStateAndAPauseWaitsBehindIt() {
        SyncQueue.Lease lease = queue.lease(CH, "P1", Duration.ofSeconds(120));
        assertEquals(SyncQueue.Lease.Kind.LEASED, lease.kind);
        assertEquals(10, lease.task.quantity);
        assertEquals(SyncQueue.Lease.Kind.BUSY, queue.lease(CH, "P1", Duration.ofSeconds(120)).kind, "one writer for each listing");
        pause(); // during the publish
        List<SyncTask> due = queue.claim(50, Duration.ofSeconds(30));
        assertEquals(1, due.size(), "P2 only: the task of P1 waits for the lease");
        queue.succeeded(due.get(0).taskId);
        queue.succeeded(lease.task.taskId); // the upsert sent 10
        assertEquals(Map.of("P1", 0), pushAll(), "the pause reaches the channel after the upsert");
    }

    @Test
    void aPublishLeaseDuringAPauseCarriesZero() {
        paused.add(STORE);
        SyncQueue.Lease lease = queue.lease(CH, "P2", Duration.ofSeconds(120));
        assertEquals(0, lease.task.quantity);
    }

    @Test
    void aNewListingNeedsNoLeaseAndAReleaseCostsNoAttempt() {
        Listing fresh = new Listing("P5", CH);
        fresh.state = ListingState.PENDING;
        store.saveListing(fresh);
        assertEquals(SyncQueue.Lease.Kind.NONE, queue.lease(CH, "P5", Duration.ofSeconds(120)).kind, "no external id: no other writer");
        SyncQueue.Lease lease = queue.lease(CH, "P1", Duration.ofSeconds(120));
        queue.released(lease.task.taskId);
        SyncTask back = store.task(lease.task.taskId).orElse(null);
        assertNotNull(back);
        assertEquals(SyncTask.State.PENDING, back.state);
        assertEquals(0, back.attempts);
        atp.put("P1", 8);
        List<Integer> sent = new ArrayList<>(pushAll().values());
        assertEquals(List.of(8), sent, "the stock job sends the value of its own moment");
    }

    // ---------------------------------------------------------------- review round 1

    @Test
    void twoWritersOfOneListingHaveOneWinnerInBothOrders() {
        SyncQueue a = new SyncQueue(store, clock, stock); // worker of JVM 1
        SyncQueue b = new SyncQueue(store, clock, stock); // worker of JVM 2
        SyncQueue.Lease first = a.lease(CH, "P1", Duration.ofSeconds(120));
        assertEquals(SyncQueue.Lease.Kind.LEASED, first.kind);
        assertEquals(SyncQueue.Lease.Kind.BUSY, b.lease(CH, "P1", Duration.ofSeconds(120)).kind, "review C3: A first, B loses");
        a.succeeded(first.task.taskId);
        SyncQueue.Lease second = b.lease(CH, "P1", Duration.ofSeconds(120));
        assertEquals(SyncQueue.Lease.Kind.LEASED, second.kind);
        assertEquals(SyncQueue.Lease.Kind.BUSY, a.lease(CH, "P1", Duration.ofSeconds(120)).kind, "review C3: B first, A loses");
        paused.add(STORE);
        sync.stockChanged("P1");
        assertTrue(a.claim(50, Duration.ofSeconds(30)).stream().noneMatch(t -> t.productId.equals("P1")),
                "a stock push of the listing waits for the lease of the publish");
    }

    @Test
    void aWriterThatDoesNotSeeTheTaskOfTheOtherStillLoses() {
        // the second writer read the tasks before the first saved its own (another JVM, an open transaction): the open == null path
        ChannelStore blind = (ChannelStore) java.lang.reflect.Proxy.newProxyInstance(getClass().getClassLoader(),
                new Class<?>[] {ChannelStore.class}, (p, m, args) -> m.getName().startsWith("openTasks") ? new ArrayList<SyncTask>()
                        : m.invoke(store, args));
        SyncQueue.Lease first = queue.lease(CH, "P1", Duration.ofSeconds(120));
        assertEquals(SyncQueue.Lease.Kind.LEASED, first.kind);
        assertEquals(SyncQueue.Lease.Kind.BUSY, new SyncQueue(blind, clock, stock).lease(CH, "P1", Duration.ofSeconds(120)).kind,
                "review C3: the compare and set on the listing decides, not the list of tasks");
        clock.advance(Duration.ofSeconds(121));
        assertEquals(SyncQueue.Lease.Kind.LEASED, new SyncQueue(blind, clock, stock).lease(CH, "P1", Duration.ofSeconds(120)).kind,
                "a lease that ended is free");
    }

    @Test
    void aNewListingGetsTheQuantityOfTheStateWithoutALease() {
        Listing fresh = new Listing("P5", CH);
        fresh.state = ListingState.PENDING;
        store.saveListing(fresh);
        atp.put("P5", 9);
        assertEquals(Integer.valueOf(9), queue.lease(CH, "P5", Duration.ofSeconds(120)).quantity);
        paused.add(STORE);
        SyncQueue.Lease l = queue.lease(CH, "P5", Duration.ofSeconds(120));
        assertEquals(SyncQueue.Lease.Kind.NONE, l.kind);
        assertEquals(Integer.valueOf(0), l.quantity, "review C4: a new listing during a pause goes out with 0");
    }
}

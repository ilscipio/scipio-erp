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
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.time.Duration;
import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * The test of the W1-08 row of the blueprint: "Stock change reaches a test channel in 60 s or less."
 * The test channel is {@link FakeChannel}. The clock is simulated; the hub loop polls every 5 s.
 */
public class StockChangeReachesChannelTest {
    private static final Duration LIMIT = Duration.ofSeconds(60);
    private Fixture f;

    @BeforeEach
    void setUp() {
        f = new Fixture();
        f.atp.put("P1", 40);
        f.live("P1", "ebay-us", "US-1", 28);
        f.live("P1", "ebay-de", "DE-1", 40);
        f.live("P1", "amazon-de", "AZ-1", 35);
    }

    @Test
    void stockChangeReachesEveryChannelWithinAMinute() {
        f.atp.put("P1", 12); // a stock change
        List<SyncTask> tasks = f.sync.stockChanged("P1");
        assertEquals(3, tasks.size());

        f.runUntil(LIMIT);

        // ATP 12: eBay US 12 - 2 = 10, eBay DE 12, Amazon DE 12 - 5 = 7
        assertEquals(10, f.channel.quantityOf("ebay-us", "US-1"));
        assertEquals(12, f.channel.quantityOf("ebay-de", "DE-1"));
        assertEquals(7, f.channel.quantityOf("amazon-de", "AZ-1"));
        for (FakeChannel.Push p : f.channel.pushes) {
            assertTrue(f.secondsSinceT0(p.at) <= 60, p.channelId + " took " + f.secondsSinceT0(p.at) + " s");
        }
        assertEquals(0, f.queue.claim(10, SyncQueue.DEFAULT_LEASE).size(), "no task left");
        assertEquals(Integer.valueOf(10), f.store.listing("P1", "ebay-us").get().lastPushedQuantity);
    }

    @Test
    void stockRuleCapsAndNeverGoesBelowZero() {
        f.atp.put("P1", 100);
        f.sync.stockChanged("P1");
        f.runUntil(LIMIT);
        assertEquals(30, f.channel.quantityOf("ebay-us", "US-1"), "max 30");
        f.atp.put("P1", 1);
        f.sync.stockChanged("P1");
        f.runUntil(Duration.ofSeconds(200));
        assertEquals(0, f.channel.quantityOf("amazon-de", "AZ-1"), "ATP 1 - buffer 5 is 0, not negative");
    }

    @Test
    void channelFailsFourTimesAndTheStockStillArrivesWithinAMinute() {
        for (int i = 0; i < 4; i++) {
            f.channel.failNext(FakeChannel.unavailable());
        }
        f.atp.put("P1", 12);
        f.sync.stockChanged("P1");
        f.runUntil(LIMIT);
        assertEquals(4, f.channel.failedCalls);
        assertEquals(10, f.channel.quantityOf("ebay-us", "US-1"));
        assertEquals(12, f.channel.quantityOf("ebay-de", "DE-1"));
        assertEquals(7, f.channel.quantityOf("amazon-de", "AZ-1"));
        for (FakeChannel.Push p : f.channel.pushes) {
            assertTrue(f.secondsSinceT0(p.at) <= 60, p.channelId + " took " + f.secondsSinceT0(p.at) + " s");
        }
    }

    @Test
    void rateLimitWaitTimeIsKept() {
        f.channel.failNext(FakeChannel.rateLimited(Duration.ofSeconds(20)));
        f.atp.put("P1", 12);
        f.sync.stockChanged("P1");
        f.runUntil(LIMIT);
        FakeChannel.Push limited = null;
        for (FakeChannel.Push p : f.channel.pushes) {
            if (p.channelId.equals("ebay-us")) {
                limited = p;
            }
        }
        assertNotNull(limited);
        assertTrue(f.secondsSinceT0(limited.at) >= 20, "the push waits 20 s after the 429");
        assertTrue(f.secondsSinceT0(limited.at) <= 60);
    }

    @Test
    void twoChangesInARowSendOnlyTheLastValue() {
        f.atp.put("P1", 30);
        f.sync.stockChanged("P1");
        f.atp.put("P1", 20);
        f.sync.stockChanged("P1");
        assertEquals(3, f.store.openTasks().size(), "one open task for each listing");
        f.runUntil(LIMIT);
        assertEquals(3, f.channel.pushes.size());
        assertEquals(20, f.channel.quantityOf("ebay-de", "DE-1"));
    }

    @Test
    void anUnchangedValueCreatesNoTask() {
        f.atp.put("P1", 40); // eBay DE knows 40, eBay US knows 28 (30 cap), Amazon DE knows 35
        f.sync.stockChanged("P1");
        assertEquals(1, f.store.openTasks().size(), "only eBay US differs: 40 - 2 = 38 capped to 30, known 28");
        assertEquals("ebay-us", f.store.openTasks().get(0).channelId);
    }

    @Test
    void aStockChangeBackToTheKnownValueCancelsThePendingPush() {
        f.atp.put("P1", 12);
        f.sync.stockChanged("P1");
        f.atp.put("P1", 40);
        f.sync.stockChanged("P1");
        f.runUntil(LIMIT);
        assertEquals(1, f.channel.pushes.size(), "only eBay US (known 28, now 30 by the cap)");
        assertNull(f.channel.quantityOf("ebay-de", "DE-1"));
    }

    @Test
    void aFailureForGoodWritesAFixHintOnTheListing() {
        f.channel.failNext(FakeChannel.notFound());
        f.atp.put("P1", 12);
        f.sync.stockChanged("P1");
        f.runUntil(LIMIT);
        int failed = 0;
        for (SyncTask t : f.store.allTasks()) {
            if (t.state == SyncTask.State.FAILED) {
                failed++;
            }
        }
        assertEquals(1, failed);
        Listing broken = null;
        for (String ch : new String[] {"ebay-us", "ebay-de", "amazon-de"}) {
            Listing l = f.store.listing("P1", ch).get();
            if (l.fixHint != null) {
                broken = l;
            }
        }
        assertNotNull(broken);
        assertTrue(broken.fixHint.contains("List the product again"));
        assertTrue(broken.errorsJson.contains("STOCK_SYNC_FAILED"));
    }

    @Test
    void aListingThatIsNotLiveGetsNoStock() {
        Listing draft = new Listing("P2", "ebay-us");
        f.store.saveListing(draft);
        f.atp.put("P2", 5);
        assertTrue(f.sync.stockChanged("P2").isEmpty());
    }

    @Test
    void aChannelOrderLowersTheStockOnTheOtherChannelsWithinAMinute() {
        f.store.productBySku.put("SKU-1", "P1");
        OrderIntake intake = new OrderIntake(f.store, (setting, order, lines) -> {
            for (OrderIntake.ResolvedLine l : lines) {
                f.atp.merge(l.productId, -l.line.quantity, Integer::sum); // the store order reserves the stock
            }
            return "ORD-1";
        }, f.sync);
        IncomingOrder order = SampleOrders.paid("EB-1", "USD", "SKU-1", 6);

        OrderIntake.Result r = intake.intake("ebay-us", order);
        assertEquals(OrderIntake.Status.CREATED, r.status);
        assertEquals("ORD-1", r.orderId);
        assertEquals(34, f.atp.get("P1"));

        f.runUntil(LIMIT);
        assertEquals(30, f.channel.quantityOf("ebay-us", "US-1"), "34 - 2, capped to 30");
        assertEquals(34, f.channel.quantityOf("ebay-de", "DE-1"));
        assertEquals(29, f.channel.quantityOf("amazon-de", "AZ-1"), "34 - 5");
    }
}

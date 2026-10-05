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
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.time.Duration;
import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/** Tests for the code-review fixes: races, retention, cancel, listing errors. */
public class ReviewFixesTest {
    private Fixture f;

    @BeforeEach
    void setUp() {
        f = new Fixture();
        f.atp.put("P1", 40);
        f.live("P1", "ebay-de", "DE-1", 40);
    }

    @Test
    void aClaimReadsTheStockAgain() {
        f.atp.put("P1", 12);
        f.sync.stockChanged("P1"); // task with 12
        f.atp.put("P1", 7); // the stock changes again, the planner does not run
        List<SyncTask> claimed = f.queue.claim(10, SyncQueue.DEFAULT_LEASE);
        assertEquals(1, claimed.size());
        assertEquals(7, claimed.get(0).quantity);
    }

    @Test
    void aClaimWhoseValueTheChannelKnowsIsClosedWithoutAPush() {
        f.atp.put("P1", 12);
        f.sync.stockChanged("P1");
        f.atp.put("P1", 40); // back to the known value, planner not run
        assertEquals(0, f.queue.claim(10, SyncQueue.DEFAULT_LEASE).size());
        assertTrue(f.store.openTasks().isEmpty());
    }

    @Test
    void twoClaimsNeverTakeTheSameTask() {
        f.atp.put("P1", 12);
        f.sync.stockChanged("P1");
        SyncTask stale = f.store.openTasks().get(0); // a second worker read the task before the first claimed it
        assertEquals(1, f.queue.claim(10, SyncQueue.DEFAULT_LEASE).size());
        SyncTask before = stale.copy();
        stale.claim(Fixture.T0, Duration.ofSeconds(30));
        assertTrue(!f.store.saveTaskIfUnchanged(stale, before), "compare and set refuses the second claim");
    }

    @Test
    void aPlannerThatLosesTheRaceDoesNotOverwriteAClaimedTask() {
        f.atp.put("P1", 12);
        f.sync.stockChanged("P1");
        SyncTask task = f.store.openTasks().get(0);
        // a worker claims the task after the planner has read it: the planner reads again and makes a new task
        f.queue.claim(10, SyncQueue.DEFAULT_LEASE);
        f.atp.put("P1", 20);
        f.sync.stockChanged("P1");
        int claimed = 0;
        int pending = 0;
        for (SyncTask t : f.store.openTasks()) {
            if (t.state == SyncTask.State.CLAIMED) {
                claimed++;
                assertEquals(task.taskId, t.taskId);
            } else {
                pending++;
            }
        }
        assertEquals(1, claimed);
        assertEquals(1, pending);
    }

    @Test
    void aNewExternalIdResetsTheLastPushedQuantity() {
        Listing l = f.store.listing("P1", "ebay-de").get();
        assertEquals(Integer.valueOf(40), l.lastPushedQuantity);
        l.changeExternalId("DE-1"); // same id: stays
        assertEquals(Integer.valueOf(40), l.lastPushedQuantity);
        l.changeExternalId("DE-2");
        assertNull(l.lastPushedQuantity);
    }

    @Test
    void aStockSuccessKeepsTheErrorsOfTheListing() {
        Listing l = f.store.listing("P1", "ebay-de").get();
        l.errorsJson = "[{\"code\":\"MISSING_ATTRIBUTE\"}]";
        l.fixHint = "Add the brand.";
        f.store.saveListing(l);
        f.atp.put("P1", 12);
        f.sync.stockChanged("P1");
        f.runUntil(Duration.ofSeconds(30));
        Listing after = f.store.listing("P1", "ebay-de").get();
        assertEquals(Integer.valueOf(12), after.lastPushedQuantity);
        assertEquals("Add the brand.", after.fixHint);
    }

    @Test
    void aStockFailureDoesNotOverwriteOtherErrorsAndASuccessClearsItsOwn() {
        f.channel.failNext(FakeChannel.notFound());
        f.atp.put("P1", 12);
        f.sync.stockChanged("P1");
        f.runUntil(Duration.ofSeconds(30));
        assertTrue(f.store.listing("P1", "ebay-de").get().errorsJson.contains("STOCK_SYNC_FAILED"));
        f.atp.put("P1", 5);
        f.sync.stockChanged("P1");
        f.runUntil(Duration.ofSeconds(60));
        Listing l = f.store.listing("P1", "ebay-de").get();
        assertNull(l.errorsJson);
        assertNull(l.fixHint);
    }

    // ---- retention ----

    @Test
    void buyerDeletionReachesAChannelThatIsSwitchedOff() {
        f.store.saveSetting(new ChannelSetting("ebay", "gb", "STORE_EBAY_GB", "ACC_GB", "GBP", true, "EBAY_CHANNEL", null,
                ChannelSetting.RetentionFrom.CLOSED, false));
        OrderRef ref = new OrderRef("ORD-9", "ebay-gb", "X-9");
        ref.buyerExternalId = "user-1";
        f.store.saveOrderRef(ref);
        List<String> erased = new ArrayList<>();
        RetentionRun run = new RetentionRun(f.store, erased::add, f.clock);
        assertEquals(1, run.buyerDeletion("ebay", "user-1").erased);
        assertEquals(1, erased.size());
    }

    @Test
    void eachEraseRunsInItsOwnTransactionAndAFailureLeavesTheOthers() {
        f.store.productBySku.put("SKU-1", "P1");
        OrderIntake intake = new OrderIntake(f.store, (s, o, l) -> "ORD-" + o.externalOrderId, f.sync);
        f.add("amazon", "gb", "STORE_AZ_GB", "GBP", true, 30);
        f.live("P1", "amazon-de", "AZ-1", 35);
        intake.intake("amazon-de", SampleOrders.order("A1", "SHIPPED", "EUR", "SKU-1", 1, "b1"));
        intake.intake("amazon-de", SampleOrders.order("A2", "SHIPPED", "EUR", "SKU-1", 1, "b2"));
        f.clock.advance(Duration.ofDays(31));
        final int[] txCount = {0};
        RetentionRun.Transactional tx = new RetentionRun.Transactional() {
            @Override
            public <T> T run(java.util.concurrent.Callable<T> work) throws Exception {
                txCount[0]++;
                return work.call();
            }
        };
        RetentionRun run = new RetentionRun(f.store, id -> {
            if (id.equals("ORD-A1")) {
                throw new IllegalStateException("boom");
            }
        }, f.clock, tx);
        RetentionRun.Result r = run.run();
        assertEquals(2, txCount[0]);
        assertEquals(1, r.erased);
        assertEquals(List.of("ORD-A1"), r.failedOrderIds);
        assertNull(f.store.orderRef("amazon-de", "A1").get().buyerDataErasedDate, "the failed order stays due");
    }

    // ---- order intake ----

    private static final class Creator implements OrderIntake.OrderCreator {
        final List<String> cancelled = new ArrayList<>();
        final Fixture f;

        Creator(Fixture f) {
            this.f = f;
        }

        @Override
        public String create(ChannelSetting s, IncomingOrder o, List<OrderIntake.ResolvedLine> lines) {
            for (OrderIntake.ResolvedLine l : lines) {
                f.atp.merge(l.productId, -l.line.quantity, Integer::sum);
            }
            return "ORD-" + o.externalOrderId;
        }

        @Override
        public void cancel(String orderId) {
            cancelled.add(orderId);
            f.atp.merge("P1", 5, Integer::sum); // the reservation goes back
        }
    }

    @Test
    void aCancelUpdateCancelsTheOpenStoreOrderAndTheStockReturns() {
        f.store.productBySku.put("SKU-1", "P1");
        Creator creator = new Creator(f);
        OrderIntake intake = new OrderIntake(f.store, creator, f.sync);
        assertEquals(OrderIntake.Status.CREATED, intake.intake("ebay-de", SampleOrders.order("E1", "PAID", "EUR", "SKU-1", 5, "b")).status);
        assertEquals(35, f.atp.get("P1"));
        OrderIntake.Result r = intake.intake("ebay-de", SampleOrders.order("E1", "CANCELLED", "EUR", "SKU-1", 5, "b"));
        assertEquals(OrderIntake.Status.DUPLICATE, r.status);
        assertEquals(List.of("ORD-E1"), creator.cancelled);
        assertEquals(40, f.atp.get("P1"));
        assertTrue(f.store.orderRef("ebay-de", "E1").get().closedDate != null);
        // a repeat of the cancel does nothing
        intake.intake("ebay-de", SampleOrders.order("E1", "CANCELLED", "EUR", "SKU-1", 5, "b"));
        assertEquals(1, creator.cancelled.size());
    }

    @Test
    void aNewRefundedOrderMakesNoStoreOrder() {
        f.store.productBySku.put("SKU-1", "P1");
        Creator creator = new Creator(f);
        OrderIntake intake = new OrderIntake(f.store, creator, f.sync);
        assertEquals(OrderIntake.Status.IGNORED, intake.intake("ebay-de", SampleOrders.order("E2", "REFUNDED", "EUR", "SKU-1", 1, "b")).status);
        assertEquals(40, f.atp.get("P1"));
    }

    @Test
    void theRefIsTheLockAndAFailedCreateReleasesIt() {
        f.store.productBySku.put("SKU-1", "P1");
        // a parallel call holds the placeholder
        assertTrue(f.store.reserveOrderRef("ebay-de", "E3") != null);
        OrderIntake intake = new OrderIntake(f.store, new Creator(f), f.sync);
        OrderIntake.Result busy = intake.intake("ebay-de", SampleOrders.paid("E3", "EUR", "SKU-1", 1));
        assertEquals(OrderIntake.Status.DUPLICATE, busy.status);
        assertNull(busy.orderId);
        assertEquals(40, f.atp.get("P1"), "no second order");

        OrderIntake failing = new OrderIntake(f.store, (s, o, l) -> {
            throw new IllegalStateException("no address");
        }, f.sync);
        assertEquals(OrderIntake.Status.REJECTED, failing.intake("ebay-de", SampleOrders.paid("E4", "EUR", "SKU-1", 1)).status);
        assertTrue(!f.store.orderRef("ebay-de", "E4").isPresent(), "the placeholder is gone");
    }
}

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
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.math.BigDecimal;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * W1-07 part 1: the event outbox. A new order writes its event in the order transaction, and a claim returns it at once.
 * Acknowledge, release, lease expiry, idempotence and purge. The tests use the in-memory store with a transaction model
 * (rollback restores the events) and a clock that the test moves by hand.
 */
public class EventOutboxTest {
    private MutableClock clock;
    private MemoryOutboxStore store;
    private EventOutbox outbox;

    @BeforeEach
    void setUp() {
        clock = new MutableClock(Instant.parse("2026-10-01T10:00:00Z"));
        store = new MemoryOutboxStore();
        outbox = new EventOutbox(store, clock);
    }

    /** The order table of the fake: the order and its event are in one transaction (MemoryOutboxStore.inTransaction). */
    private final List<String> orders = new ArrayList<>();

    private OutboxEvents.OrderFacts order(String id, String type, String status, boolean physical) {
        OutboxEvents.OrderFacts f = new OutboxEvents.OrderFacts();
        f.orderId = id;
        f.orderTypeId = type;
        f.statusId = status;
        f.productStoreId = "STORE_A";
        f.salesChannelEnumId = "WEB_SALES_CHANNEL";
        f.grandTotal = new BigDecimal("59.90");
        f.currencyUom = "USD";
        f.itemCount = 2;
        f.needsShipping = physical;
        return f;
    }

    /** Fake of the service storeOrder with its commit ECA: writes the order, then the event, in one transaction. */
    private void storeOrder(OutboxEvents.OrderFacts f, boolean failAfterEvent) {
        store.inTransaction(() -> {
            orders.add(f.orderId);
            OutboxEvents.orderCreated(outbox, f);
            if (failAfterEvent) {
                throw new IllegalStateException("commit failed");
            }
            return null;
        });
    }

    // ---- gate: new order -> event in the order transaction -> claim returns it at once ----

    @Test
    void newOrderWritesAnEventInTheOrderTransactionAndAClaimReturnsItAtOnce() {
        storeOrder(order("10000", "SALES_ORDER", "ORDER_CREATED", true), false);
        assertEquals(Collections.singletonList("10000"), orders);
        assertEquals(1, store.size());

        List<OutboxEvent> got = outbox.claim("desk-1", 10, Duration.ofSeconds(30), null);
        assertEquals(1, got.size());
        OutboxEvent e = got.get(0);
        assertEquals(OutboxEvents.ORDER_CREATED, e.eventType);
        assertEquals("desk-1", e.claimedBy);
        assertEquals(1, e.attempts);
        assertTrue(e.payloadJson.contains("\"orderId\":\"10000\""), e.payloadJson);
        assertTrue(e.payloadJson.contains("\"grandTotal\":59.90"), e.payloadJson);
        assertTrue(e.payloadJson.contains("\"productStoreId\":\"STORE_A\""), e.payloadJson);
        assertTrue(e.payloadJson.contains("\"channelId\":null"), e.payloadJson); // the own store
    }

    @Test
    void aRolledBackOrderLeavesNoEvent() {
        assertThrows(IllegalStateException.class, () -> storeOrder(order("10001", "SALES_ORDER", "ORDER_CREATED", true), true));
        assertEquals(0, store.size());
        assertTrue(outbox.claim("desk-1", 10, null, null).isEmpty());
    }

    @Test
    void aChannelOrderCarriesItsChannel() {
        OutboxEvents.OrderFacts f = order("10002", "SALES_ORDER", "ORDER_CREATED", true);
        f.channelId = "ebay-us";
        f.salesChannelEnumId = "EBAY_SALES_CHANNEL";
        storeOrder(f, false);
        assertTrue(outbox.claim("d", 1, null, null).get(0).payloadJson.contains("\"channelId\":\"ebay-us\""));
    }

    @Test
    void aPurchaseOrderWritesNoEvent() {
        storeOrder(order("10003", "PURCHASE_ORDER", "ORDER_CREATED", true), false);
        assertEquals(0, store.size());
    }

    @Test
    void approvalOfAnOrderWithPhysicalItemsNeedsShippingOnceOnly() {
        OutboxEvents.OrderFacts f = order("10004", "SALES_ORDER", "ORDER_CREATED", true);
        storeOrder(f, false);
        assertEquals(1, store.size());
        f.statusId = "ORDER_APPROVED";
        assertEquals(1, OutboxEvents.orderApproved(outbox, f));
        assertEquals(0, OutboxEvents.orderApproved(outbox, f), "a repeat writes no second event");
        assertEquals(2, store.size());
        List<String> types = new ArrayList<>();
        outbox.claim("d", 10, null, null).forEach(e -> types.add(e.eventType));
        assertEquals(Arrays.asList(OutboxEvents.ORDER_CREATED, OutboxEvents.ORDER_NEEDS_SHIPPING), types);
    }

    @Test
    void anOrderApprovedAtCreationWritesBothEvents() {
        storeOrder(order("10005", "SALES_ORDER", "ORDER_APPROVED", true), false);
        assertEquals(2, store.size());
    }

    @Test
    void aDigitalOrderNeedsNoShipping() {
        OutboxEvents.OrderFacts f = order("10006", "SALES_ORDER", "ORDER_APPROVED", false);
        storeOrder(f, false);
        assertEquals(1, store.size());
        assertEquals(0, OutboxEvents.orderApproved(outbox, f));
    }

    @Test
    void aStockChangeWritesAnEventWithItsKey() {
        OutboxEvents.StockFacts s = new OutboxEvents.StockFacts();
        s.productId = "P1";
        s.inventoryItemId = "9000";
        s.inventoryItemDetailSeqId = "0001";
        s.availableToPromiseDiff = new BigDecimal("-2");
        s.quantityOnHandDiff = BigDecimal.ZERO;
        assertEquals(1, OutboxEvents.stockChanged(outbox, s));
        assertEquals(0, OutboxEvents.stockChanged(outbox, s), "the same detail row writes one event");
        s.inventoryItemDetailSeqId = "0002";
        assertEquals(1, OutboxEvents.stockChanged(outbox, s));
        s.productId = null;
        assertEquals(0, OutboxEvents.stockChanged(outbox, s));
        OutboxEvent e = outbox.claim("d", 1, null, null).get(0);
        assertEquals(OutboxEvents.STOCK_CHANGED, e.eventType);
        assertTrue(e.payloadJson.contains("\"availableToPromiseDiff\":-2"), e.payloadJson);
    }

    // ---- claim, lease ----

    @Test
    void claimGivesOldestFirstUpToTheLimitAndHoldsTheEvents() {
        for (int i = 0; i < 5; i++) {
            outbox.record("T", null, Collections.singletonMap("n", i));
            clock.advance(Duration.ofSeconds(1));
        }
        List<OutboxEvent> a = outbox.claim("desk-1", 3, Duration.ofSeconds(60), null);
        assertEquals(3, a.size());
        assertTrue(a.get(0).payloadJson.contains("\"n\":0") && a.get(2).payloadJson.contains("\"n\":2"));
        List<OutboxEvent> b = outbox.claim("desk-2", 10, Duration.ofSeconds(60), null);
        assertEquals(2, b.size(), "the held events are not given to a second consumer");
        assertTrue(outbox.claim("desk-3", 10, null, null).isEmpty());
    }

    @Test
    void anExpiredLeaseGivesTheEventAgainAndCountsAnAttempt() {
        outbox.record("T", null, Collections.singletonMap("n", 1));
        OutboxEvent first = outbox.claim("desk-1", 1, Duration.ofSeconds(30), null).get(0);
        clock.advance(Duration.ofSeconds(29));
        assertTrue(outbox.claim("desk-2", 1, Duration.ofSeconds(30), null).isEmpty(), "the lease runs");
        clock.advance(Duration.ofSeconds(1));
        List<OutboxEvent> again = outbox.claim("desk-2", 1, Duration.ofSeconds(30), null);
        assertEquals(1, again.size());
        assertEquals(first.eventId, again.get(0).eventId, "the same event id: the consumer deduplicates by it");
        assertEquals(2, again.get(0).attempts);
        assertEquals("desk-2", again.get(0).claimedBy);
    }

    @Test
    void theOldHolderCannotAckAfterAnotherConsumerTookTheEvent() {
        outbox.record("T", null, null);
        String id = outbox.claim("desk-1", 1, Duration.ofSeconds(30), null).get(0).eventId;
        clock.advance(Duration.ofSeconds(31));
        outbox.claim("desk-2", 1, Duration.ofSeconds(30), null);
        EventOutbox.Outcome o = outbox.ack("desk-1", Collections.singletonList(id));
        assertEquals(Collections.singletonList(id), o.notOwner);
        assertTrue(o.done.isEmpty());
        assertTrue(outbox.ack("desk-2", Collections.singletonList(id)).done.contains(id));
    }

    @Test
    void claimFiltersByType() {
        outbox.record("A", null, null);
        outbox.record("B", null, null);
        List<OutboxEvent> got = outbox.claim("d", 10, null, new HashSet<>(Collections.singletonList("B")));
        assertEquals(1, got.size());
        assertEquals("B", got.get(0).eventType);
    }

    // ---- acknowledge ----

    @Test
    void ackClosesTheEventAndARepeatIsSafe() {
        outbox.record("T", null, null);
        String id = outbox.claim("desk-1", 1, null, null).get(0).eventId;
        EventOutbox.Outcome o = outbox.ack("desk-1", Arrays.asList(id, "NOPE"));
        assertEquals(Collections.singletonList(id), o.done);
        assertEquals(Collections.singletonList("NOPE"), o.unknown);
        assertNotNull(store.event(id).get().doneDate);
        clock.advance(Duration.ofHours(1));
        assertTrue(outbox.claim("desk-1", 10, null, null).isEmpty(), "a done event is never given again");
        assertEquals(Collections.singletonList(id), outbox.ack("desk-1", Collections.singletonList(id)).done, "a repeat is a success");
    }

    // ---- release ----

    @Test
    void releaseGivesTheEventBackAtOnceAndKeepsTheAttempts() {
        outbox.record("T", null, null);
        String id = outbox.claim("desk-1", 1, Duration.ofMinutes(5), null).get(0).eventId;
        EventOutbox.Outcome o = outbox.release("desk-1", Collections.singletonList(id), "APNs 503", null);
        assertEquals(Collections.singletonList(id), o.done);
        OutboxEvent e = store.event(id).get();
        assertNull(e.claimedBy);
        assertEquals("APNs 503", e.lastError);
        List<OutboxEvent> again = outbox.claim("desk-2", 1, null, null);
        assertEquals(1, again.size(), "back at once, before the old lease would end");
        assertEquals(2, again.get(0).attempts);
    }

    @Test
    void releaseWithAWaitTimeHidesTheEventForThatTime() {
        outbox.record("T", null, null);
        String id = outbox.claim("desk-1", 1, null, null).get(0).eventId;
        outbox.release("desk-1", Collections.singletonList(id), "slow down", Duration.ofSeconds(20));
        assertTrue(outbox.claim("desk-1", 1, null, null).isEmpty());
        clock.advance(Duration.ofSeconds(20));
        assertEquals(1, outbox.claim("desk-1", 1, null, null).size());
    }

    @Test
    void aConsumerCannotReleaseWhatAnotherHolds() {
        outbox.record("T", null, null);
        String id = outbox.claim("desk-1", 1, Duration.ofSeconds(30), null).get(0).eventId;
        EventOutbox.Outcome o = outbox.release("desk-2", Collections.singletonList(id), "x", null);
        assertEquals(Collections.singletonList(id), o.notOwner);
        assertEquals("desk-1", store.event(id).get().claimedBy);
    }

    // ---- purge ----

    @Test
    void purgeDeletesDoneEventsAfterTheRetentionOnly() {
        String oldDone = recordDone();
        clock.advance(Duration.ofDays(6));
        String recentDone = recordDone();
        outbox.record("T", null, Collections.singletonMap("open", true)); // open, old by the end
        clock.advance(Duration.ofDays(2)); // oldDone is 8 days old, recentDone 2 days
        assertEquals(1, outbox.purge(Duration.ofDays(EventOutbox.DEFAULT_RETENTION_DAYS)));
        assertFalse(store.event(oldDone).isPresent());
        assertTrue(store.event(recentDone).isPresent());
        clock.advance(Duration.ofDays(30));
        assertEquals(1, outbox.purge(Duration.ofDays(7)), "the other done event goes, the open event stays");
        assertEquals(1, store.size());
        assertNull(store.all().get(0).doneDate);
    }

    private String recordDone() {
        outbox.record("T", null, new LinkedHashMap<>());
        OutboxEvent e = outbox.claim("d", 1, null, null).get(0);
        outbox.ack("d", Collections.singletonList(e.eventId));
        return e.eventId;
    }
}

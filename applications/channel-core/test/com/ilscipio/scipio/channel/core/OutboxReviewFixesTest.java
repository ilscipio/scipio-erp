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
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.time.Duration;
import java.time.Instant;
import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/** W1-07 review fixes: duplicate write, owner check during a retry wait, attempts limit. */
public class OutboxReviewFixesTest {
    private MutableClock clock;
    private MemoryOutboxStore store;
    private EventOutbox outbox;

    @BeforeEach
    void setUp() {
        clock = new MutableClock(Instant.parse("2026-10-01T10:00:00Z"));
        store = new MemoryOutboxStore();
        outbox = new EventOutbox(store, clock);
    }

    // finding 1 (the row lock itself needs a database; the memory store shows that a duplicate is a quiet no-op)
    @Test
    void aDuplicateWriteIsAQuietNoOp() {
        assertNotNull(outbox.record("ORDER_NEEDS_SHIPPING", "ORDER_NEEDS_SHIPPING:1", Collections.singletonMap("orderId", "1")));
        assertNull(store.inTransaction(() -> outbox.record("ORDER_NEEDS_SHIPPING", "ORDER_NEEDS_SHIPPING:1", Collections.singletonMap("orderId", "1"))));
        assertEquals(1, store.size());
    }

    // finding 3
    @Test
    void aReleasedEventInItsRetryWaitIsNotOwnedByAnyCaller() {
        String id = outbox.record("ORDER_CREATED", "k1", Collections.singletonMap("orderId", "1"));
        outbox.claim("desk-1", 1, Duration.ofSeconds(30), null);
        assertEquals(1, outbox.release("desk-1", Collections.singletonList(id), "boom", Duration.ofSeconds(60)).done.size());
        EventOutbox.Outcome ack = outbox.ack("desk-2", Collections.singletonList(id));
        assertEquals(Collections.singletonList(id), ack.notOwner);
        assertEquals(Collections.singletonList(id), outbox.release("desk-2", Collections.singletonList(id), "x", null).notOwner);
        assertTrue(outbox.claim("desk-2", 1, null, null).isEmpty(), "the wait still runs");
        clock.advance(Duration.ofSeconds(61));
        assertEquals(Collections.singletonList(id), outbox.ack("desk-2", Collections.singletonList(id)).done, "the wait is over");
    }

    // finding 4
    @Test
    void anEventThatReachesMaxAttemptsIsParkedAndNotClaimedAgain() {
        outbox = new EventOutbox(store, clock, 3);
        String id = outbox.record("ORDER_CREATED", "k2", Collections.singletonMap("orderId", "2"));
        for (int i = 1; i <= 3; i++) {
            assertEquals(1, outbox.claim("desk-1", 1, Duration.ofSeconds(5), null).size());
            outbox.release("desk-1", Collections.singletonList(id), "fail " + i, null);
        }
        assertTrue(outbox.claim("desk-1", 1, null, null).isEmpty());
        List<OutboxEvent> parked = outbox.parked(10);
        assertEquals(1, parked.size());
        assertEquals(id, parked.get(0).eventId);
        assertEquals("fail 3", parked.get(0).lastError);
        assertTrue(parked.get(0).isParked());
        assertEquals(0, outbox.purge(Duration.ZERO), "purge keeps a parked event");
        assertEquals(1, store.size());
    }

    @Test
    void anEventBelowMaxAttemptsIsNotParked() {
        outbox = new EventOutbox(store, clock, 3);
        String id = outbox.record("ORDER_CREATED", "k3", Collections.singletonMap("orderId", "3"));
        outbox.claim("desk-1", 1, null, null);
        outbox.release("desk-1", Collections.singletonList(id), "fail", null);
        assertFalse(store.event(id).get().isParked());
        assertEquals(1, outbox.claim("desk-1", 1, null, null).size());
    }
}

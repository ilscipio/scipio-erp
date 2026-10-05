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

/**
 * One row of the event outbox (entity ChannelOutboxEvent). The store writes it in the transaction of its cause.
 * A consumer (the desk) claims it for a lease time, handles it, and acknowledges it. A lease that expires frees the event.
 *
 * <p>SCIPIO: 4.0.0: Added (W1-07).</p>
 */
public final class OutboxEvent {
    public final String eventId;
    public final String eventType;
    public final String payloadJson;
    public final Instant createdDate;
    /** Key that makes a write idempotent: a second event with the same key is not written. Equals the event id when the cause has no natural key. */
    public final String dedupeKey;
    public String claimedBy;
    public Instant claimedDate;
    /** While in the future the event is held by {@link #claimedBy}, or (no claimer) waits for a retry. */
    public Instant leaseUntil;
    public Instant doneDate;
    public int attempts;
    public String lastError;
    /** Set when the attempts reached the maximum: the event is parked (no claim, no purge) until the desk acts on it. */
    public Instant parkedDate;

    public OutboxEvent(String eventId, String eventType, String payloadJson, String dedupeKey, Instant createdDate) {
        this.eventId = Listing.req(eventId, "eventId");
        this.eventType = Listing.req(eventType, "eventType");
        this.payloadJson = payloadJson == null ? "{}" : payloadJson;
        this.dedupeKey = dedupeKey == null || dedupeKey.isEmpty() ? eventId : dedupeKey;
        this.createdDate = createdDate;
    }

    public boolean isDone() {
        return doneDate != null;
    }

    /** Open, and no lease runs at {@code now}. */
    public boolean isClaimable(Instant now) {
        return doneDate == null && parkedDate == null && (leaseUntil == null || !leaseUntil.isAfter(now));
    }

    public boolean isParked() {
        return doneDate == null && parkedDate != null;
    }

    /** Held by a consumer at {@code now}. */
    public boolean isLeased(Instant now) {
        return doneDate == null && claimedBy != null && leaseUntil != null && leaseUntil.isAfter(now);
    }

    public OutboxEvent copy() {
        OutboxEvent c = new OutboxEvent(eventId, eventType, payloadJson, dedupeKey, createdDate);
        c.claimedBy = claimedBy;
        c.claimedDate = claimedDate;
        c.leaseUntil = leaseUntil;
        c.doneDate = doneDate;
        c.attempts = attempts;
        c.lastError = lastError;
        c.parkedDate = parkedDate;
        return c;
    }
}

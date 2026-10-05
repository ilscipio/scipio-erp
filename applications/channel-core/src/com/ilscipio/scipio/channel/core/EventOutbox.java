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

import java.math.BigDecimal;
import java.time.Clock;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

/**
 * The event outbox of a store. The store writes events ({@link #record}) in the transaction of the cause. A consumer
 * (the desk, through the MCP topic "channel") claims a batch for a lease time ({@link #claim}), then acknowledges
 * ({@link #ack}) or releases ({@link #release}) each event. Delivery is at least once: an event whose lease expires,
 * or that is released, is given again. A consumer makes its handling idempotent by the event id.
 *
 * <p>Rules: (1) A claim is a compare and set, so two consumers never hold the same event. (2) An acknowledge of an event that is
 * done already is a success (a repeat is safe). (3) A consumer cannot acknowledge or release an event that another consumer
 * holds under a running lease. (4) {@link #purge} deletes done events only.</p>
 *
 * <p>SCIPIO: 4.0.0: Added (W1-07).</p>
 */
public final class EventOutbox {
    public static final Duration DEFAULT_LEASE = Duration.ofSeconds(60);
    public static final int DEFAULT_RETENTION_DAYS = 7;
    /** An event whose attempts reach this number is parked (property outbox.maxAttempts). */
    public static final int DEFAULT_MAX_ATTEMPTS = 10;

    private final OutboxStore store;
    private final Clock clock;
    private final int maxAttempts;

    public EventOutbox(OutboxStore store, Clock clock) {
        this(store, clock, DEFAULT_MAX_ATTEMPTS);
    }

    public EventOutbox(OutboxStore store, Clock clock, int maxAttempts) {
        this.store = store;
        this.clock = clock;
        this.maxAttempts = maxAttempts < 1 ? DEFAULT_MAX_ATTEMPTS : maxAttempts;
    }

    /** Result of an acknowledge or release call: ids by outcome. */
    public static final class Outcome {
        public final List<String> done = new ArrayList<>();
        public final List<String> unknown = new ArrayList<>();
        public final List<String> notOwner = new ArrayList<>();
    }

    /**
     * Writes an event. {@code dedupeKey} (may be null) makes the write idempotent. Returns the event id, or null when an event
     * with the key exists already.
     */
    public String record(String eventType, String dedupeKey, Map<String, ?> payload) {
        String id = store.nextId();
        OutboxEvent e = new OutboxEvent(id, eventType, json(payload), dedupeKey, clock.instant());
        return store.append(e) ? id : null;
    }

    /** Claims up to {@code limit} events, oldest first. Each claim counts one attempt. */
    public List<OutboxEvent> claim(String consumerId, int limit, Duration lease, Set<String> types) {
        Listing.req(consumerId, "consumerId");
        Instant now = clock.instant();
        Duration l = lease == null ? DEFAULT_LEASE : lease;
        List<OutboxEvent> claimed = new ArrayList<>();
        // A lost compare and set skips the event; read more than the limit so that a busy store still fills the batch.
        for (OutboxEvent e : store.claimable(now, types, Math.max(1, limit) * 2)) {
            if (claimed.size() >= limit) {
                break;
            }
            OutboxEvent before = e.copy();
            if (e.attempts >= maxAttempts) {
                // A poison event whose lease expired again and again (the consumer never released it): park it, do not claim it.
                e.parkedDate = now;
                e.lastError = e.lastError == null ? "Parked: " + e.attempts + " attempts" : e.lastError;
                store.saveIfUnchanged(e, before);
                continue;
            }
            e.claimedBy = consumerId;
            e.claimedDate = now;
            e.leaseUntil = now.plus(l);
            e.attempts++;
            if (store.saveIfUnchanged(e, before)) {
                claimed.add(e);
            }
        }
        return claimed;
    }

    /** The consumer handled the events. A repeat is a success. */
    public Outcome ack(String consumerId, Collection<String> eventIds) {
        Instant now = clock.instant();
        Outcome out = new Outcome();
        for (String id : eventIds) {
            Optional<OutboxEvent> opt = store.event(id);
            if (!opt.isPresent()) {
                out.unknown.add(id);
                continue;
            }
            OutboxEvent e = opt.get();
            if (e.isDone()) {
                out.done.add(id);
                continue;
            }
            if (heldByAnother(e, consumerId, now)) {
                out.notOwner.add(id);
                continue;
            }
            OutboxEvent before = e.copy();
            e.doneDate = now;
            e.leaseUntil = null;
            e.lastError = null;
            if (store.saveIfUnchanged(e, before)) {
                out.done.add(id);
            } else {
                out.notOwner.add(id); // another worker changed it in the meantime
            }
        }
        return out;
    }

    /**
     * The consumer could not handle the events. They go back to the list at once, or after {@code retryAfter}. The attempt count stays.
     */
    public Outcome release(String consumerId, Collection<String> eventIds, String error, Duration retryAfter) {
        Instant now = clock.instant();
        Outcome out = new Outcome();
        for (String id : eventIds) {
            Optional<OutboxEvent> opt = store.event(id);
            if (!opt.isPresent()) {
                out.unknown.add(id);
                continue;
            }
            OutboxEvent e = opt.get();
            if (e.isDone() || heldByAnother(e, consumerId, now)) {
                out.notOwner.add(id);
                continue;
            }
            OutboxEvent before = e.copy();
            e.claimedBy = null;
            e.leaseUntil = retryAfter == null || retryAfter.isZero() || retryAfter.isNegative() ? null : now.plus(retryAfter);
            if (error != null && !error.isEmpty()) {
                e.lastError = error.length() > 255 ? error.substring(0, 255) : error;
            }
            if (e.attempts >= maxAttempts) {
                e.parkedDate = now; // the event failed maxAttempts times: stop the retry loop
            }
            if (store.saveIfUnchanged(e, before)) {
                out.done.add(id);
            } else {
                out.notOwner.add(id);
            }
        }
        return out;
    }

    /** Parked events (attempts reached the maximum), oldest first. The desk lists them to decide what to do. */
    public List<OutboxEvent> parked(int limit) {
        return store.parked(Math.max(1, limit));
    }

    /** Deletes the events that are done for longer than {@code retention}. Returns the count. */
    public int purge(Duration retention) {
        return store.purgeDoneBefore(clock.instant().minus(retention));
    }

    private static boolean heldByAnother(OutboxEvent e, String consumerId, Instant now) {
        if (e.doneDate != null || e.leaseUntil == null || !e.leaseUntil.isAfter(now)) {
            return false;
        }
        // A running lease with no claimer is a retry wait after a release: no consumer owns the event then.
        return e.claimedBy == null || !e.claimedBy.equals(consumerId);
    }

    // ---- minimal JSON writer for payload maps (strings, numbers, booleans, null, nested maps and lists) ----

    static String json(Map<String, ?> map) {
        StringBuilder sb = new StringBuilder();
        value(sb, map == null ? new LinkedHashMap<String, Object>() : map);
        return sb.toString();
    }

    private static void value(StringBuilder sb, Object v) {
        if (v == null) {
            sb.append("null");
        } else if (v instanceof BigDecimal) {
            sb.append(((BigDecimal) v).toPlainString());
        } else if (v instanceof Number || v instanceof Boolean) {
            sb.append(v);
        } else if (v instanceof Map) {
            sb.append('{');
            boolean first = true;
            for (Map.Entry<?, ?> en : ((Map<?, ?>) v).entrySet()) {
                if (!first) {
                    sb.append(',');
                }
                first = false;
                string(sb, String.valueOf(en.getKey()));
                sb.append(':');
                value(sb, en.getValue());
            }
            sb.append('}');
        } else if (v instanceof Collection) {
            sb.append('[');
            boolean first = true;
            for (Object o : (Collection<?>) v) {
                if (!first) {
                    sb.append(',');
                }
                first = false;
                value(sb, o);
            }
            sb.append(']');
        } else {
            string(sb, v.toString());
        }
    }

    private static void string(StringBuilder sb, String s) {
        sb.append('"');
        for (char c : s.toCharArray()) {
            if (c == '"' || c == '\\') {
                sb.append('\\').append(c);
            } else if (c < 0x20) {
                sb.append(' ');
            } else {
                sb.append(c);
            }
        }
        sb.append('"');
    }
}

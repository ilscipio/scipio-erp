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
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Supplier;

/**
 * In-memory {@link OutboxStore} with a transaction model: {@link #inTransaction} keeps a snapshot and restores it when the work
 * throws, like a database rollback. The events and the orders of {@code OrderEventsTest} share one transaction this way.
 */
public class MemoryOutboxStore implements OutboxStore {
    private Map<String, OutboxEvent> events = new LinkedHashMap<>();
    private int seq = 10000;

    @Override
    public String nextId() {
        return "EVT" + (seq++);
    }

    @Override
    public boolean append(OutboxEvent e) {
        for (OutboxEvent o : events.values()) {
            if (o.dedupeKey.equals(e.dedupeKey)) {
                return false;
            }
        }
        events.put(e.eventId, e.copy());
        return true;
    }

    @Override
    public Optional<OutboxEvent> event(String eventId) {
        OutboxEvent e = events.get(eventId);
        return e == null ? Optional.empty() : Optional.of(e.copy());
    }

    @Override
    public List<OutboxEvent> claimable(Instant now, Set<String> types, int limit) {
        List<OutboxEvent> out = new ArrayList<>();
        events.values().stream()
                .filter(e -> e.isClaimable(now))
                .filter(e -> types == null || types.isEmpty() || types.contains(e.eventType))
                .sorted(Comparator.comparing((OutboxEvent e) -> e.createdDate).thenComparing(e -> e.eventId))
                .limit(limit)
                .forEach(e -> out.add(e.copy()));
        return out;
    }

    @Override
    public List<OutboxEvent> parked(int limit) {
        List<OutboxEvent> out = new ArrayList<>();
        events.values().stream().filter(OutboxEvent::isParked)
                .sorted(Comparator.comparing((OutboxEvent e) -> e.createdDate).thenComparing(e -> e.eventId))
                .limit(limit).forEach(e -> out.add(e.copy()));
        return out;
    }

    @Override
    public boolean saveIfUnchanged(OutboxEvent e, OutboxEvent before) {
        OutboxEvent cur = events.get(e.eventId);
        if (cur == null || cur.attempts != before.attempts || !java.util.Objects.equals(cur.doneDate, before.doneDate)
                || !java.util.Objects.equals(cur.leaseUntil, before.leaseUntil) || !java.util.Objects.equals(cur.claimedBy, before.claimedBy)
                || !java.util.Objects.equals(cur.parkedDate, before.parkedDate)) {
            return false;
        }
        events.put(e.eventId, e.copy());
        return true;
    }

    @Override
    public int purgeDoneBefore(Instant cutoff) {
        int before = events.size();
        events.values().removeIf(e -> e.doneDate != null && e.doneDate.isBefore(cutoff));
        return before - events.size();
    }

    public int size() {
        return events.size();
    }

    public List<OutboxEvent> all() {
        List<OutboxEvent> out = new ArrayList<>();
        events.values().forEach(e -> out.add(e.copy()));
        return out;
    }

    /** Runs the work; when it throws, the events return to the state before the call. */
    public <T> T inTransaction(Supplier<T> work) {
        Map<String, OutboxEvent> snapshot = new LinkedHashMap<>();
        events.forEach((k, v) -> snapshot.put(k, v.copy()));
        int seqBefore = seq;
        try {
            return work.get();
        } catch (RuntimeException ex) {
            events = snapshot;
            seq = seqBefore;
            throw ex;
        }
    }
}

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
import java.util.Set;

/**
 * Storage port of the event outbox. The entity implementation is {@code EntityOutboxStore}; the tests use an in-memory one.
 * Returned objects are copies.
 *
 * <p>SCIPIO: 4.0.0: Added (W1-07).</p>
 */
public interface OutboxStore {

    String nextId();

    /** Writes a new event. Returns false (and writes nothing) when an event with the same dedupe key exists. */
    boolean append(OutboxEvent e);

    Optional<OutboxEvent> event(String eventId);

    /** Open events without a running lease, oldest first, of the given types (null or empty: all types). */
    List<OutboxEvent> claimable(Instant now, Set<String> types, int limit);

    /** Parked events (attempts reached the maximum), oldest first. */
    List<OutboxEvent> parked(int limit);

    /**
     * Compare and set: saves {@code e} only when the stored event still has the done date, claimer, attempts and lease of
     * {@code before}. Returns false when another worker changed it.
     */
    boolean saveIfUnchanged(OutboxEvent e, OutboxEvent before);

    /** Deletes the events that are done before {@code cutoff}. Returns the count. */
    int purgeDoneBefore(Instant cutoff);
}

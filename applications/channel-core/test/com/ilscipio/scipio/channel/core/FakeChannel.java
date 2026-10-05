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

import java.time.Clock;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * The test channel of W1-08: a fake marketplace that holds the quantity of each listing. It can fail the next calls
 * (the faults of the connector contract: 503 retryable, 429 with a wait time, 404 or a bad request for good).
 */
public class FakeChannel implements StockGateway {
    public static final class Push {
        public final String channelId;
        public final String externalId;
        public final int quantity;
        public final Instant at;

        Push(String channelId, String externalId, int quantity, Instant at) {
            this.channelId = channelId;
            this.externalId = externalId;
            this.quantity = quantity;
            this.at = at;
        }
    }

    private final Clock clock;
    private final Map<String, Integer> quantities = new HashMap<>();
    private final Deque<GatewayException> faults = new ArrayDeque<>();
    public final List<Push> pushes = new ArrayList<>();
    public int failedCalls;

    public FakeChannel(Clock clock) {
        this.clock = clock;
    }

    public void failNext(GatewayException e) {
        faults.add(e);
    }

    public static GatewayException unavailable() {
        return new GatewayException("HTTP 503", true, null, null);
    }

    public static GatewayException rateLimited(Duration retryAfter) {
        return new GatewayException("HTTP 429", true, retryAfter, null);
    }

    public static GatewayException notFound() {
        return new GatewayException("Listing not found", false, null, "The listing is gone on the channel. List the product again.");
    }

    public Integer quantityOf(String channelId, String externalId) {
        return quantities.get(channelId + "|" + externalId);
    }

    public Instant lastPushAt(String channelId, String externalId) {
        Instant at = null;
        for (Push p : pushes) {
            if (p.channelId.equals(channelId) && p.externalId.equals(externalId)) {
                at = p.at;
            }
        }
        return at;
    }

    @Override
    public void updateStock(String channelId, String externalId, int quantity) throws GatewayException {
        if (!faults.isEmpty()) {
            failedCalls++;
            throw faults.poll();
        }
        quantities.put(channelId + "|" + externalId, quantity);
        pushes.add(new Push(channelId, externalId, quantity, clock.instant()));
    }
}

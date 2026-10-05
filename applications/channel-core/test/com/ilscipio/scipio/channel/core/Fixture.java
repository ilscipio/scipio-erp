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
import java.time.Duration;
import java.time.Instant;
import java.util.HashMap;
import java.util.Map;

/** A store with three channels (eBay US, eBay DE, Amazon DE), one stock, and a fake channel. */
class Fixture {
    static final Instant T0 = Instant.parse("2026-10-01T10:00:00Z");
    static final Duration TICK = Duration.ofSeconds(5);

    final MutableClock clock = new MutableClock(T0);
    final MemoryChannelStore store = new MemoryChannelStore();
    final Map<String, Integer> atp = new HashMap<>();
    final FakeChannel channel = new FakeChannel(clock);
    final StockSource stock = (p, storeId) -> BigDecimal.valueOf(atp.getOrDefault(p, 0));
    final StockSync sync = new StockSync(store, stock, clock);
    final SyncQueue queue = new SyncQueue(store, clock, stock);
    final ChannelDispatcher dispatcher = new ChannelDispatcher(queue, channel);

    Fixture() {
        add("ebay", "us", "STORE_EBAY_US", "USD", false, null);
        add("ebay", "de", "STORE_EBAY_DE", "EUR", true, null);
        add("amazon", "de", "STORE_AMZ_DE", "EUR", true, 30);
        store.saveStockRule("ebay-us", new StockRule(2, 30));
        store.saveStockRule("amazon-de", new StockRule(5, null));
    }

    ChannelSetting add(String connector, String market, String storeId, String currency, boolean incl, Integer retentionDays) {
        ChannelSetting s = new ChannelSetting(connector, market, storeId, "ACC_" + connector + "_" + market, currency, incl,
                connector.toUpperCase() + "_CHANNEL", retentionDays, ChannelSetting.RetentionFrom.CLOSED, true);
        store.saveSetting(s);
        return s;
    }

    /** A live listing that the channel knows with the given quantity. */
    Listing live(String productId, String channelId, String externalId, Integer known) {
        Listing l = new Listing(productId, channelId);
        l.externalId = externalId;
        l.state = ListingState.LIVE;
        l.lastPushedQuantity = known;
        store.saveListing(l);
        return l;
    }

    /** Runs the hub loop: one tick every 5 s until {@code until} after T0. */
    void runUntil(Duration until) {
        while (Duration.between(T0, clock.instant()).compareTo(until) <= 0) {
            dispatcher.runDue(50);
            clock.advance(TICK);
        }
    }

    long secondsSinceT0(Instant at) {
        return Duration.between(T0, at).getSeconds();
    }
}
